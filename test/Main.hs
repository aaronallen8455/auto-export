module Main (main) where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified System.Directory as Dir
import qualified System.Process as Proc

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "Class1" $ runTest "Class1.hs"
  , testCase "Data1" $ runTest "Data1.hs"
  , testCase "Data2" $ runTest "Data2.hs"
  , testCase "Data3" $ runTest "Data3.hs"
  , testCase "Data4" $ runTest "Data4.hs"
  , testCase "KindSig1" $ runTest "KindSig1.hs"
  , testCase "Pat1" $ runTest "Pat1.hs"
  , testCase "Pat2" $ runTest "Pat2.hs"
  , testCase "Ty1" $ runTest "Ty1.hs"
  , testCase "Ty2" $ runTest "Ty2.hs"
  , testCase "Ty3" $ runTest "Ty3.hs"
  , testCase "Ty4" $ runTest "Ty4.hs"
  , testCase "Ty5" $ runTest "Ty5.hs"
  , testCase "Ty6" $ runTest "Ty6.hs"
  , testCase "Var1" $ runTest "Var1.hs"
  , testCase "Var2" $ runTest "Var2.hs"
  , testCase "Var3" $ runTest "Var3.hs"
  , testCase "Var4" $ runTest "Var4.hs"
  , testCase "Var5" $ runTest "Var5.hs"
  ]

testModulePath :: String -> FilePath
testModulePath name = "test-modules/" <> name

-- copy the input file contents to the module file to be compiled
prepTest :: FilePath -> IO ()
prepTest modFile = do
  inp <- readFile (modFile ++ ".input")
  writeFile modFile inp

runTest :: FilePath -> Assertion
runTest name = do
  let modFile = testModulePath name
  prepTest modFile
  (_, _, _, h) <- Proc.createProcess $
    Proc.proc "cabal" ["build", "test-modules:" ++ takeWhile (/= '.') name]
  void $ Proc.waitForProcess h
  updatedMod <- readFile modFile
  expectedMod <- readFile $ modFile ++ ".expected"
  assertEqual "Expected update" expectedMod updatedMod
  Dir.removeFile modFile
