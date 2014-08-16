module Main where
import Test.Framework
import Parser
import TypeCheck
import Data.Monoid

main :: IO ()
main = defaultMainWithOpts [parserTest, typeCheckTest] mempty
