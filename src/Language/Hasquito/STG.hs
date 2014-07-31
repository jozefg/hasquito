module Language.Hasquito.STG where
import Language.Hasquito.Syntax

-- | The top level that all declarations will be
-- compiled to.
data TopLevel = Thunk SExp
              | Fun [Name] SExp

-- | The new expression language, includes
-- primitive ints, variables, and full and partial
-- application.
data SExp = SNum Int
          | SVar Name
          | SApp SExp [SExp]
          | FullApp Op SExp SExp

data InfoTable = InfoTable { arity   :: Int
                           , codePtr :: SExp }

data Closure = Closure { freeVars :: [SExp]
                       , table    :: InfoTable }

