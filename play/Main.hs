{-# LANGUAGE PatternSynonyms #-}
module Main
  ( main, pattern P
  ) where

main :: IO ()
main = pure ()

test :: Int
test = 1

data Foo = Foo

pattern P = ()
