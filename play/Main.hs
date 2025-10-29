{-# LANGUAGE PatternSynonyms #-}
module Main
  ( main, (#)
  ) where

main :: IO ()
main = pure ()

test :: Int
test = 1

data Foo = Foo

a # b = a + b
