{-# LANGUAGE OverloadedStrings #-}
module TypeCheck where
import Language.Hasquito
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

a :: Ty
a = TVar Nothing Rigid (Name "a")

b :: Ty
b = TVar Nothing Rigid (Name "b")

x :: Name
x = Name "x"

y :: Name
y = Name "y"

literals :: [Def]
literals = [ Def TNum (Name "one") (Num 1)
           , Def TNum (Name "two") (Num 2)
           , Def TNum (Name "ten") (Num 10) ]

recursion :: [Def]
recursion = [ Def (TVar Nothing Rigid (Name "a")) (Name "foo") (Var (Name "foo"))
            , Def (TArr TNum TNum)                (Name "bar") (Var (Name "bar"))]

polymorphism :: [Def]
polymorphism = [ Def (TArr a (TArr b a)) (Name "const") (Lam [] x (Lam [] y (Var x)))
               , Def (TArr a a)          (Name "ident") (Lam [] x (Var x)) ]

application :: [Def]
application = [ Def (TArr a (TArr b a)) (Name "const") (Lam [] x (Lam [] y (Var x)))
              , Def (TArr a a)          (Name "ident") (Lam [] x (Var x))
              , Def TNum                (Name "app1")  (App (Var (Name "ident")) (Num 1))
              , Def TNum                (Name "app2")  (App (App (Var (Name "const")) (Num 1)) (Num 2))]

doTest :: String -> [Def] -> Test
doTest name defs = testCase name $
                   runCompilerM' (typeCheck defs) @=? Right defs
  where runCompilerM' ds = case runCompilerM ds of
          Left _ -> Left ()
          Right r -> Right r

typeCheckTest :: Test
typeCheckTest = testGroup "Type Check"
                [ doTest "Literals" literals
                , doTest "Recursion" recursion
                , doTest "Polymorphism" polymorphism
                , doTest "Application" application]
