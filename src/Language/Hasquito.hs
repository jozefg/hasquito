module Language.Hasquito (
  module H,
  compileFile) where
import Control.Monad
import Language.Hasquito.Closure as H
import Language.Hasquito.DeExp as H
import Language.Hasquito.JSify
import Language.Hasquito.MakeMain
import Language.Hasquito.Parser as H
import Language.Hasquito.STG as H
import Language.Hasquito.Syntax as H
import Language.Hasquito.TypeCheck as H
import Language.Hasquito.Uniquify as H
import Language.Hasquito.Util as H
import Language.Hasquito.Sanity as H
import Language.JavaScript.AST
import Language.JavaScript.Pretty as J

mainCompiler :: [Def] -> CompilerM Program
mainCompiler = typeCheck
               >=> sanityCheck
               >=> simplify
               >=> toSTG
               >=> deExp
               >=> uniquify
               >=> jsify
               >=> makeMain

compileFile :: FilePath -> IO ()
compileFile file = do
  parseRes <- parseFile file
  let compRes = parseRes >>= runCompilerM . mainCompiler
  either print (print . J.pretty) compRes
