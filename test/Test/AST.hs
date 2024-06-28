module Test.AST (astTests) where

import Test.Tasty
import Test.Tasty.HUnit

import AST

astTests :: TestTree
astTests = testGroup "AST"
    [
        evalTests
    ]

evalTests :: TestTree
evalTests =
    let genCase desc input expected = testCase desc $ evalAst input @?= Right expected
    in
    testGroup "AST.evalAst" [
        genCase
            "Integer negation (from spec)"
            (Negate (Number 3))
            (Number (-3))
    ,   genCase
            "Boolean not (from spec)"
            (Not (Boolean True))
            (Boolean False)
    ,   genCase
            "String-to-int (from spec)"
            (StrToInt (Str "test"))
            (Number 15818151)
    ,   genCase
            "Int-to-string (from spec)"
            (IntToStr (Number 15818151))
            (Str "test")
    ,   genCase
            "Integer addition (from spec)"
            (Add (Number 2) (Number 3))
            (Number 5)
    ,   genCase
            "Integer subtraction (from spec)"
            (Sub (Number 3) (Number 2))
            (Number 1)
    ,   genCase
            "Integer multiplication (from spec)"
            (Mult (Number 3) (Number 2))
            (Number 6)
    ,   genCase
            "Integer division with negation (from spec)"
            (Div (Negate (Number 7)) (Number 2))
            (Number (-3))
    ,   genCase
            "Integer modulo with negation (from spec)"
            (Mod (Negate (Number 7)) (Number 2))
            (Number (-1))
    ,   genCase
            "Less-than integer comparison (from spec)"
            (Lt (Number 3) (Number 2))
            (Boolean False)
    ,   genCase
            "Greater-than integer comparison (from spec)"
            (Gt (Number 3) (Number 2))
            (Boolean True)
    ,   genCase
            "Integer equality (from spec)"
            (Equals (Number 3) (Number 2))
            (Boolean False)
    ,   genCase
            "String equality"
            (Equals (Str "hello") (Str "world"))
            (Boolean False)
    ,   genCase
            "Boolean equality"
            (Equals (Boolean False) (Boolean False))
            (Boolean True)
    ,   genCase
            "Boolean or (from spec)"
            (Or (Boolean True) (Boolean False))
            (Boolean True)
    ,   genCase
            "Boolean and (from spec)"
            (And (Boolean True) (Boolean False))
            (Boolean False)
    ,   genCase
            "String concatenation (from spec)"
            (Concat (Str "te") (Str "st"))
            (Str "test")
    ,   genCase
            "Take first x chars of string y (from spec)"
            (Take (Number 3) (Str "test"))
            (Str "tes")
    ,   genCase
            "Drop first x chars of string y (from spec)"
            (Drop (Number 3) (Str "test"))
            (Str "t")
    ,   genCase
            "If (from spec)"
            (If (Gt (Number 2) (Number 3)) (Str "yes") (Str "no"))
            (Str "no")
    ,   genCase
            "Lambda abstractions (from spec)"
            (Apply (Apply (Lambda 2 (Lambda 3 (Var 2))) (Concat (Str "Hello") (Str " World!"))) (Number 42))
            (Str "Hello World!")
    ,   genCase
            "Evaluation example from spec"
            (Apply (Lambda 2 (Apply (Lambda 1 (Add (Var 1) (Var 1))) (Mult (Number 3) (Number 2)))) (Var 23))
            (Number 12)
    ]
