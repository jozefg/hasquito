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
import Language.Hasquito.Util as H
import Language.Hasquito.Sanity as H
import Language.JavaScript.AST
import Language.JavaScript.Pretty as J
import System.Exit

mainCompiler :: [Def] -> CompilerM Program
mainCompiler = typeCheck
               >=> sanityCheck
               >=> simplify
               >=> toSTG
               >=> deExp
               >=> jsify
               >=> makeMain

compileFile :: FilePath -> IO String
compileFile file = do
  parseRes <- parseFile file
  let compRes = parseRes >>= runCompilerM . mainCompiler
  case compRes of
    Right prog -> return . show . J.pretty $ prog
    Left err   -> print err >> exitWith (ExitFailure 1)
