module Language.Hasquito.STG where
import Language.Hasquito.Syntax

data SExp = SNum Int
          | SVar Name
          | SApp SExp [SExp]
          | FullApp Op SExp SExp
          | App SExp [SExp]

