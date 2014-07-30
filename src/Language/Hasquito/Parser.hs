{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.Parser where
import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Language.Hasquito.Syntax
import           Language.Hasquito.Util

name :: Parser Name
name = Name . T.pack <$> many1 letter

tparen :: Parser Ty
tparen = char '(' *> ty <* char ')'

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
ty = skipSpace *> (try tarr <|> tparen <|> tnum <|> tvar) <* skipSpace

num :: Parser Exp
num = Num <$> signed decimal

var :: Parser Exp
var = Var <$> name

app :: Parser Exp
app = do
  l <- expr
  char ' ' *> skipSpace
  r <- expr
  return (App l r)

bindings :: Parser [(Name, Ty)]
bindings = many1 (skipSpace *> binding)
  where binding = do
          char '(' *> skipSpace 
          n <- name
          skipSpace *> char ':'
          t <- ty
          return (n, t)

lam :: Parser Exp
lam = do
  char '('
  vars <- bindings
  skipSpace *> string " -> " *> skipSpace
  body <- expr
  char ')'
  return (Lam vars body)

eparen :: Parser Exp
eparen = char '(' *> expr <* skipSpace <* char ')'

expr :: Parser Exp
expr = skipSpace *> (eparen <|> try app <|> try lam <|> var <|> num)

def :: Parser (Def ())
def = do
  skipSpace
  nm <- name
  skipSpace *> char ':' *> skipSpace
  t <- ty
  skipSpace *> char '=' *> skipSpace
  ex <- expr
  skipSpace <* char ';'
  return (Def t nm ex ())

file :: Parser [Def ()]
file = many def <* skipSpace

parseFile :: FilePath -> IO (Either Error [Def ()])
parseFile path = mapL (ParseError . T.pack)  . parseOnly file <$> (TIO.readFile path)
  where mapL f (Left a)  = Left (f a)
        mapL _ (Right b) = Right b
