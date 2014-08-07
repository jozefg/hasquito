module Language.Hasquito (
  module H,
  compileFile) where
import Language.Hasquito.Parser as H
import Language.Hasquito.Syntax as H
import Language.Hasquito.TypeCheck as H
import Language.Hasquito.Util as H
import Language.Hasquito.Closure as H
import Language.Hasquito.STG as H
import Control.Monad

mainCompiler :: [Def ()] -> CompilerM [TopLevel]
mainCompiler = typeCheck >=> simplify >=> toSTG

compileFile :: FilePath -> IO ()
compileFile file = do
  parseRes <- parseFile file
  print parseRes
  putStrLn "\n\n\n"
  let compRes = parseRes >>= runCompilerM . mainCompiler
  either print print compRes
