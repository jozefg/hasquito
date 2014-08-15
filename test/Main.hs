module Main where
import Test.Framework
import Parser
import Data.Monoid

main :: IO ()
main = defaultMainWithOpts [parserTest] mempty
