module Main where
import Language.Hasquito
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs
  decs <- parseFile f
  let res = decs >>= runCompilerM . typeCheck
  case res of
    Left errors -> print errors
    Right _     -> putStrLn "It typechecks"
