{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.Parser where
import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Language.Hasquito.Syntax
import           Language.Hasquito.Util
import           Data.List (foldl1')

name :: Parser Name
name = Name . T.pack <$> many1 letter

tparen :: Parser Ty
tparen = char '(' *> ty <* char ')'

tnum :: Parser Ty
tnum = string "Num" *> return TNum

tvar :: Parser Ty
tvar = TVar Nothing Rigid <$> name

ty :: Parser Ty
ty = do
  t <- nonrec
  try (TArr t <$> (string "->" *> ty)) <|> return t
  where nonrec = skipSpace *> (tparen <|> tnum <|> tvar) <* skipSpace

num :: Parser Exp
num = Num <$> signed decimal

var :: Parser Exp
var = Var <$> name

lam :: Parser Exp
lam = do
  string "fun "
  v:vars <- reverse <$> many1 (skipSpace *> name)
  skipSpace <* string "->"
  body <- expr
  return $ foldr (Lam []) (Lam [] v body) vars

eparen :: Parser Exp
eparen = char '(' *> expr <* skipSpace <* char ')'

prim :: Parser Exp
prim = Op <$> (    char '+' *> return Plus
               <|> char '-' *> return Minus
               <|> char '*' *> return Mult
               <|> char '/' *> return Div)

ifz :: Parser Exp
ifz = do
  string "if" *> skipSpace
  n <- nonrecExp <* skipSpace
  l <- nonrecExp <* skipSpace
  r <- nonrecExp <* skipSpace
  return (IfZ n l r)

nonrecExp :: Parser Exp
nonrecExp = skipSpace *> (try lam <|> ifz <|> eparen <|> prim <|> var <|> num)

expr :: Parser Exp
expr = foldl1' App <$> many1 nonrecExp


def :: Parser Def
def = do
  skipSpace
  nm <- name
  skipSpace *> char ':' *> skipSpace
  t <- ty
  skipSpace *> char '=' *> skipSpace
  ex <- expr
  skipSpace <* char ';'
  return (Def t nm ex)

file :: T.Text -> Either String [Def]
file = parseOnly (many def <* skipSpace <* endOfInput)

parseFile :: FilePath -> IO (Either Error [Def])
parseFile path = mapL (ParseError . T.pack)  . file <$> (TIO.readFile path)
  where mapL f (Left a)  = Left (f a)
        mapL _ (Right b) = Right b
