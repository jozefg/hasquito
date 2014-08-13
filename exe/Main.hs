module Main where
import Language.Hasquito
import System.Environment
import System.IO

main :: IO ()
main = do
  [f] <- getArgs
  prog <- compileFile f
  withFile "rts/rts.js" ReadMode $ \rtsHandle ->
    withFile "out.js" WriteMode $ \outHandle -> do
      rts <- hGetContents rtsHandle
      hPutStrLn outHandle rts
      hPutStrLn outHandle prog
      
