{-# LANGUAGE OverloadedStrings #-}
module Parser (parserTest) where
import qualified Data.Text as T
import Language.Hasquito.Parser
import Language.Hasquito.Syntax
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

variableNames :: [T.Text]
variableNames = [ "UPPERCASE : Num = 1;"
                , "lowercase : Num = 1;"
                , "mixedCase : Num = 1;" ]

variableNamesRes :: [Def]
variableNamesRes = [ Def TNum (Name "UPPERCASE") (Num 1)
                   , Def TNum (Name "lowercase") (Num 1)
                   , Def TNum (Name "mixedCase") (Num 1)]

operators :: [T.Text]
operators = [ "addition  : Num = + 1 1;"
            , "subtraction : Num = - 1 1;"
            , "multiplication : Num = * 1 1;"
            , "division : Num = / 1 1;" ]

operatorsRes :: [Def]
operatorsRes = [ Def TNum (Name "addition") (App (App (Op Plus) (Num 1)) (Num 1))
               , Def TNum (Name "subtraction") (App (App (Op Minus) (Num 1)) (Num 1))
               , Def TNum (Name "multiplication") (App (App (Op Mult) (Num 1)) (Num 1))
               , Def TNum (Name "division") (App (App (Op Div) (Num 1)) (Num 1))]

functionApplication :: [T.Text]
functionApplication = [ "oneArgument : Num = + 1;"
                      , "twoArgument : Num = + 1 1;"
                      , "threeArgument : Num = + 1 1 1;"]

functionApplicationRes :: [Def]
functionApplicationRes = [ Def TNum (Name "oneArgument") (App (Op Plus) (Num 1))
                         , Def TNum (Name "twoArgument") (App (App (Op Plus) (Num 1)) (Num 1))
                         , Def TNum (Name "threeArgument") (App (App (App (Op Plus) (Num 1)) (Num 1)) (Num 1))]

doParse :: T.Text -> Maybe Def
doParse s = case file s of
  Right [d] -> Just d
  _         -> Nothing

doTest :: [T.Text] -> [Def] -> [Test]
doTest terms defs = zipWith test terms defs
  where test text res = testCase "" $ Just res @=? doParse text

parserTest :: Test
parserTest = testGroup "Parser Test"
             [ testGroup "Variable names"        (doTest variableNames variableNamesRes)
             , testGroup "Operators"             (doTest operators operatorsRes)
             , testGroup "Function Application"  (doTest functionApplication functionApplicationRes)]
