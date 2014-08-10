module Language.Hasquito (
  module H,
  compileFile) where
import Language.Hasquito.Parser as H
import Language.Hasquito.Syntax as H
import Language.Hasquito.TypeCheck as H
import Language.Hasquito.Util as H
import Language.Hasquito.Closure as H
import Language.Hasquito.STG as H
import Language.Hasquito.Uniquify as H
import Language.Hasquito.DeExp as H
import Control.Monad
import Language.JavaScript.AST
import Language.Hasquito.JSify
import Language.JavaScript.Pretty as J
import Text.PrettyPrint.Leijen

mainCompiler :: [Def ()] -> CompilerM [VarDecl]
mainCompiler = typeCheck >=> simplify >=> toSTG >=> deExp >=> uniquify >=> jsify

prettyPrint :: [VarDecl] -> IO ()
prettyPrint = print . foldr1 smush . map J.pretty
  where smush a b = a <> line <> b

compileFile :: FilePath -> IO ()
compileFile file = do
  parseRes <- parseFile file
  let compRes = parseRes >>= runCompilerM . mainCompiler
  either print prettyPrint compRes
