{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module AutoExport
  ( plugin
  ) where

import           Control.Exception
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Writer.CPS as W
import qualified Data.ByteString as BS hiding (uncons)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Char as Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.String
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import qualified Language.Haskell.GHC.ExactPrint.Utils as EP

import qualified AutoExport.GhcFacade as Ghc

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.driverPlugin = const modifyHscEnv
  , Ghc.pluginRecompile = mempty
  }

modifyHscEnv :: Ghc.HscEnv -> IO Ghc.HscEnv
modifyHscEnv hscEnv =
    pure hscEnv { Ghc.hsc_hooks = modifyHooks (Ghc.hsc_hooks hscEnv) }
  where
    isExportError msgEnv =
      case Ghc.errMsgDiagnostic msgEnv of
        Ghc.GhcPsMessage (Ghc.PsErrBangPatWithoutSpace (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ rdr))))
          | "EXPORT" <- rdrNameFS rdr -> True
        _ -> False

    modifyHooks hooks =
      let runPhaseOrExistingHook :: Ghc.TPhase res -> IO res
          runPhaseOrExistingHook = maybe Ghc.runPhase (\(Ghc.PhaseHook h) -> h)
            $ Ghc.runPhaseHook hooks
          phaseHook :: Ghc.PhaseHook
          phaseHook = Ghc.PhaseHook $ \phase -> case phase of
            Ghc.T_Hsc env modSum -> catch (runPhaseOrExistingHook phase) $
              \ (Ghc.SourceError msgs) ->
                 case any isExportError $ Ghc.getMessages msgs of
                  True | Just updatedBuffer <- modifyBuffer =<< Ghc.ms_hspp_buf modSum
                    -> do
                    let innerPlugin = mkInnerPlugin hscEnv
                        updatedModSum = modSum { Ghc.ms_hspp_buf = Just updatedBuffer }
                        staticPlugin = Ghc.StaticPlugin
                          { Ghc.spPlugin = Ghc.PluginWithArgs innerPlugin []
#if MIN_VERSION_ghc(9,12,0)
                          , Ghc.spInitialised = True
#endif
                          }
                        newEnv = env
                          { Ghc.hsc_plugins = let plugins = Ghc.hsc_plugins hscEnv in plugins
                            { Ghc.staticPlugins = staticPlugin : Ghc.staticPlugins plugins }
                          }
                    runPhaseOrExistingHook (Ghc.T_Hsc newEnv updatedModSum)
                  _ -> throw $ Ghc.SourceError msgs
            _ -> runPhaseOrExistingHook phase
       in hooks
            { Ghc.runPhaseHook = Just phaseHook }

rdrNameFS :: Ghc.RdrName -> Ghc.FastString
rdrNameFS = Ghc.occNameFS . Ghc.rdrNameOcc

modifyBuffer :: Ghc.StringBuffer -> Maybe Ghc.StringBuffer
modifyBuffer = fmap Ghc.stringBufferFromByteString . rewriteRawExtract . stringBufferToBS

stringBufferToBS :: Ghc.StringBuffer -> BS.ByteString
stringBufferToBS Ghc.StringBuffer {Ghc.buf = buf, Ghc.len = len} =
  BS.BS buf len

rewriteRawExtract :: BS.ByteString -> Maybe BS.ByteString
rewriteRawExtract = toMaybe . W.runWriter . traverse update . BS.lines
  where
    update x | isExportCmd x = W.writer ("{-# ANN EXPORT () #-}", Any True)
             | otherwise = pure x
    toMaybe (x, Any True) = Just $ BS.unlines x
    toMaybe _ = Nothing

removeExportCmds :: BS.ByteString -> BS.ByteString
removeExportCmds = BS.unlines . filter (not . isExportCmd) . BS.lines

isExportCmd :: BS.ByteString -> Bool
isExportCmd = (== "!EXPORT") . BS.strip

mkInnerPlugin :: Ghc.HscEnv -> Ghc.Plugin
mkInnerPlugin hscEnv = Ghc.defaultPlugin
  { Ghc.parsedResultAction = \_ modSum result -> do
      let newExportItems =
            getExports . Ghc.hsmodDecls . Ghc.unLoc . Ghc.hpm_module
            $ Ghc.parsedResultModule result
      case Ghc.ml_hs_file (Ghc.ms_location modSum) of
        Nothing -> pure result
        Just filePath -> do
          liftIO $ prepareSourceForParsing filePath
          let dynFlags = Ghc.ms_hspp_opts modSum `Ghc.gopt_set` Ghc.Opt_KeepRawTokenStream
          parseResult <- liftIO $ parseModule hscEnv dynFlags filePath
          case parseResult of
            (Right parsedMod, usesCpp) ->
              liftIO $ modifyModule parsedMod usesCpp newExportItems filePath
            (Left _, _) -> pure ()

          let exportErr =
                let fn = fromMaybe "<UNKNOWN>" $ Ghc.ml_hs_file (Ghc.ms_location modSum)
                 in Ghc.mkPlainErrorMsgEnvelope
                      (Ghc.mkGeneralSrcSpan $ fromString fn)
                      (Ghc.ghcUnknownMessage ExportDiag)
          Ghc.throwOneError exportErr
  }

modifyModule
  :: Ghc.ParsedSource
  -> Bool
  -> [Ghc.IE Ghc.GhcPs]
  -> FilePath
  -> IO ()
modifyModule parsedMod usesCpp newIEs filePath = do
  putStrLn . Ghc.showSDocUnsafe $ Ghc.ppr newIEs
  let ast = EP.makeDeltaAst parsedMod
      addIEs m = m
          { Ghc.hsmodExports = fmap (addExports newIEs) <$> Ghc.hsmodExports m }
      updatedMod = addIEs <$> ast
  -- If the source contains CPP, newlines are appended
  -- to the end of the file when exact printing. The simple
  -- solution is to remove trailing newlines after exact printing
  -- if the source contains CPP comments.
  let removeTrailingNewlines
        | usesCpp =
            reverse . ('\n' :) . dropWhile (== '\n') . reverse
        | otherwise = id
      printed = removeTrailingNewlines $ EP.exactPrint updatedMod
  writeFile filePath printed

addExports :: [Ghc.IE Ghc.GhcPs] -> [Ghc.LIE Ghc.GhcPs] -> [Ghc.LIE Ghc.GhcPs]
addExports [] lies = lies
addExports newIEs lies = liesWithComma ++ newIEsWithCommas
  where
    existingNames = Set.fromList $ Ghc.ieName . Ghc.unLoc <$> lies
    iesToAdd = filter ((`Set.notMember` existingNames) . Ghc.ieName) newIEs
    addComma (Ghc.L l ie) = Ghc.L (EP.addComma l) ie
    liesWithComma = case reverse lies of
      l : ls -> reverse $ addComma l : ls
      [] -> []
    newIEsWithCommas = case reverse (Ghc.L EP.noAnnSrcSpanDP1 <$> iesToAdd) of
      l : ls -> reverse $ l : (addComma <$> ls)
      [] -> []

getExports :: [Ghc.LHsDecl Ghc.GhcPs] -> [Ghc.IE Ghc.GhcPs]
getExports (Ghc.L _ a : Ghc.L locB b : rest)
  | Ghc.AnnD _ (Ghc.HsAnnotation _ prov _) <- a
  , Ghc.ValueAnnProvenance (Ghc.L _ rdrName) <- prov
  , "EXPORT" <- rdrNameFS rdrName
  , ies <- mkIE b
  = ies ++ getExports (Ghc.L locB b : rest)
  | otherwise = getExports (Ghc.L locB b : rest)
getExports _ = []

mkIE :: Ghc.HsDecl Ghc.GhcPs -> [Ghc.IE Ghc.GhcPs]
mkIE = \case
    Ghc.TyClD _ tyCl ->
      let getTyName = \case
            Ghc.FamDecl _ fd -> Ghc.fdLName fd
            t -> Ghc.tcdLName t
       in case tyCl of
            _ | Ghc.isTypeFamilyDecl tyCl -> [mkThingAbsIE (Ghc.unLoc $ getTyName tyCl)]
            Ghc.FamDecl{} -> [mkThingAbsIE (Ghc.unLoc $ getTyName tyCl)]
            Ghc.SynDecl{} -> [mkThingAbsIE (Ghc.unLoc $ getTyName tyCl)]
            _ ->
              [Ghc.IEThingAll Ghc.ieThingAllAnn
                (Ghc.L Ghc.noSrcSpanA
                  (case Ghc.unLoc $ getTyName tyCl of
                     n | isOperator n ->
                      Ghc.IEType Ghc.ieTypeAnn
                        (addOpParens $ Ghc.L EP.noAnnSrcSpanDP1 n)
                     n ->
                      Ghc.IEName Ghc.noExtField
                        (addOpParens $ Ghc.L EP.noAnnSrcSpanDP0 n)
                  )
                )
                Nothing
              ]
    Ghc.ValD _ (Ghc.FunBind _ (Ghc.L _ name) _) -> [mkVarIE name]
    Ghc.ValD _ (Ghc.PatSynBind _ psb) -> [mkPatternIE (Ghc.unLoc $ Ghc.psb_id psb)]
    Ghc.SigD _ sig -> case sig of
      Ghc.TypeSig _ names _ -> mkVarIE . Ghc.unLoc <$> names
      Ghc.PatSynSig _ names _ -> mkPatternIE . Ghc.unLoc <$> names
      _ -> []
    Ghc.KindSigD _ (Ghc.StandaloneKindSig _ (Ghc.L _ name) _) -> [mkThingAbsIE name]
    Ghc.ForD _ for -> [mkVarIE . Ghc.unLoc $ Ghc.fd_name for]
    _ -> []
  where
    mkVarIE name =
      Ghc.IEVar
        Ghc.noAnn
        (Ghc.L Ghc.noSrcSpanA
          (Ghc.IEName Ghc.noExtField . addOpParens $ Ghc.L EP.noAnnSrcSpanDP0 name)
        )
        Nothing
    mkPatternIE :: Ghc.RdrName -> Ghc.IE Ghc.GhcPs
    mkPatternIE name =
      Ghc.IEThingAbs Ghc.noAnn
        (Ghc.L Ghc.noSrcSpanA
          (Ghc.IEPattern Ghc.iePatternAnn
            (addOpParens $ Ghc.L EP.noAnnSrcSpanDP1 name)
          )
        )
        Nothing
    mkThingAbsIE :: Ghc.RdrName -> Ghc.IE Ghc.GhcPs
    mkThingAbsIE name =
      Ghc.IEThingAbs Ghc.noAnn
        (Ghc.L Ghc.noSrcSpanA
          (if isOperator name
           then
            Ghc.IEType Ghc.ieTypeAnn
              (addOpParens $ Ghc.L EP.noAnnSrcSpanDP1 name)
           else
            Ghc.IEName
              Ghc.noExtField
              (addOpParens $ Ghc.L EP.noAnnSrcSpanDP0 name)
          )
        )
        Nothing
    -- Adds parens for operators
    addOpParens :: Ghc.LIdP Ghc.GhcPs -> Ghc.LIdP Ghc.GhcPs
    addOpParens (Ghc.L loc name)
      | isOperator name =
          Ghc.L
            (loc { Ghc.anns = Ghc.nameAnnParens })
            name
      | otherwise = Ghc.L loc name

isOperator :: Ghc.RdrName -> Bool
isOperator name =
  case BS.uncons (Ghc.bytesFS $ rdrNameFS name) of
    Nothing -> False
    Just (c, _) ->
      c /= '_' && not (Char.isAlpha c)

-- | Parse the given module file. Accounts for CPP comments
parseModule
  :: Ghc.HscEnv
  -> Ghc.DynFlags
  -> FilePath
  -> IO (EP.ParseResult Ghc.ParsedSource, Bool)
parseModule env dynFlags filePath = EP.ghcWrapper Paths.libdir $ do
  Ghc.setSession env { Ghc.hsc_dflags = dynFlags }
  res <- EP.parseModuleEpAnnsWithCppInternal EP.defaultCppOptions dynFlags filePath
  let eCppComments = fmap (\(c, _, _) -> c) res
      hasCpp = case eCppComments of
                 Right cs -> not $ null cs
                 _ -> False
  pure
    ( liftA2 EP.insertCppComments
        (EP.postParseTransform res)
        eCppComments
    , hasCpp
    )

-- | Diagnostic thrown when extraction occurs
data ExportDiag = ExportDiag

instance Ghc.Diagnostic ExportDiag where
  type DiagnosticOpts ExportDiag = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "Module updated by auto-export, compilation aborted"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif


prepareSourceForParsing
  :: FilePath
  -> IO ()
prepareSourceForParsing filePath = do
  content <- BS.readFile filePath
  BS.writeFile filePath (removeExportCmds content)
