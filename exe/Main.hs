module Main where
import Language.Hasquito
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs
  compileFile f
