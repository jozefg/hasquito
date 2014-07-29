{-# LANGUAGE OverloadedStrings #-}
module Parser where
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T
import Language.Hasquito.Syntax

name :: Parser Name
name = Name . T.pack <$> many1 letter

tarr :: Parser Ty
tarr = do
  l <- ty
  _ <- string "->"
  r <- ty
  return (TArr l r)

tnum :: Parser Ty
tnum = string "Num" *> return TNum

tvar :: Parser Ty
tvar = TVar <$> name

ty :: Parser Ty
ty = skipSpace *> (tvar <|> tnum <|> tarr) <* skipSpace

num :: Parser Exp
num = Num <$> signed decimal

var :: Parser Exp
var = Var <$> name
