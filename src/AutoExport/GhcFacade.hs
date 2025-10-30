{-# LANGUAGE CPP #-}
module AutoExport.GhcFacade
  ( module Ghc
  , ieThingAllAnn
  , ieTypeAnn
  , iePatternAnn
  , nameAnnParens
  ) where

import           GHC.Driver.Plugins as Ghc
import           GHC.Driver.Env.Types as Ghc
import           GHC.Driver.Hooks as Ghc
import           GHC.Driver.Pipeline.Phases as Ghc
import           GHC.Driver.Pipeline.Execute as Ghc
import           GHC.Types.SourceError as Ghc
import           GHC.Types.Error as Ghc
import           GHC.Unit.Module.ModSummary as Ghc
import           Language.Haskell.Syntax.Expr as Ghc
import           GHC.Driver.Errors.Types as Ghc
import           GHC.Parser.Errors.Types as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Types.Name.Reader as Ghc
import           GHC.Types.Name.Occurrence as Ghc
import           GHC.Data.FastString as Ghc
import           GHC.Data.StringBuffer as Ghc
import           Language.Haskell.Syntax.Decls as Ghc
import           GHC.Hs.Extension as Ghc
import           Language.Haskell.Syntax.ImpExp as Ghc
import           GHC.Hs as Ghc
import           GHC.Driver.Monad as Ghc
import           GHC as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Utils.Error as Ghc
import           GHC.Driver.DynFlags as Ghc

import qualified Language.Haskell.GHC.ExactPrint as EP

ieThingAllAnn :: Ghc.XIEThingAll Ghc.GhcPs
ieThingAllAnn =
#if MIN_VERSION_ghc(9,12,0)
  (Nothing, (Ghc.EpTok EP.d0, Ghc.EpTok EP.d0, Ghc.EpTok EP.d0))
#else
  (Nothing, [ Ghc.AddEpAnn Ghc.AnnOpenP EP.d0
            , Ghc.AddEpAnn Ghc.AnnDotdot EP.d0
            , Ghc.AddEpAnn Ghc.AnnCloseP EP.d0
            ]
  )
#endif

ieTypeAnn :: Ghc.XIEType Ghc.GhcPs
ieTypeAnn =
#if MIN_VERSION_ghc(9,12,0)
  (Ghc.EpTok EP.d0)
#else
  EP.d0
#endif

iePatternAnn :: Ghc.XIEPattern Ghc.GhcPs
iePatternAnn =
#if MIN_VERSION_ghc(9,12,0)
  (Ghc.EpTok EP.d0)
#else
  EP.d0
#endif

nameAnnParens :: Ghc.NameAnn
nameAnnParens =
#if MIN_VERSION_ghc(9,12,0)
  Ghc.NameAnn
    { Ghc.nann_adornment = Ghc.NameParens (Ghc.EpTok EP.d0) (Ghc.EpTok EP.d0)
    , Ghc.nann_name = EP.d0
    , Ghc.nann_trailing = []
    }
#else
  Ghc.NameAnn
    { Ghc.nann_adornment = Ghc.NameParens
    , Ghc.nann_open = EP.d0
    , Ghc.nann_name = EP.d0
    , Ghc.nann_close = EP.d0
    , Ghc.nann_trailing = []
    }
#endif
