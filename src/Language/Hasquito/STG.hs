{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.STG where
import Control.Applicative
import Control.Monad.Except
import Data.Monoid
import Language.Hasquito.Syntax
import Language.Hasquito.Util

-- | The top level that all declarations will be
-- compiled to.
data TopLevel = Thunk Name SExp
              | Fun Name [Name] Name SExp
              deriving Show

-- | The new expression language, includes
-- primitive ints, variables, and full and partial
-- application. This is different then our original
-- source since it doesn't allow partially applied primitives
-- or any lambdas.
data SExp = SNum Int
          | SVar Name
          | SApp SExp SExp
          | FullApp Op Name Name
          deriving Show

convert :: Exp -> CompilerM SExp
convert (App (App (Op p) (Var n)) (Var m)) = return $ FullApp p n m
convert Op{} = throwError . Impossible $ "Unsatured operator in STG.convert"
convert (App l r) = SApp <$> convert l <*> convert r
convert (Num i) = return $ SNum i
convert (Var n) = return $ SVar n
convert l@Lam{} = throwError . Impossible $ "Unlifted lambda in STG.convert!" <> pretty l

convertDec :: Def m -> CompilerM TopLevel
convertDec (Def _ nm (Lam closed var body) _) = Fun nm closed var <$> convert body
convertDec (Def _ nm e _) = Thunk nm <$> convert e

toSTG :: [Def m] -> CompilerM [TopLevel]
toSTG = mapM convertDec
