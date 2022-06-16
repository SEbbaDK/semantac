{-# LANGUAGE NamedFieldPuns #-}
module Latex where

import Ast
import Loc

import Data.List (intercalate)

class Latex a where
        latexify :: a -> String

instance Latex a => Latex (Loc a) where
    latexify (Loc _ a) = latexify a

instance Latex Rule where
    latexify Rule { name, base, premises, properties } = unlines
        [ "\\semrule{" ++ name ++ "}:&"
        , "\\frac"
        , "{" ++ (intercalate " \\quad " $ map latexify premises) ++ "}"
        , "{" ++ latexify base ++ "}"
        ]

instance Latex Premise where
    latexify (PTransition t)     = latexify t
    latexify (PConstraint e)     = latexify e
    latexify (PDefinition e1 e2) = unwords [ latexify e1, "=", latexify e2 ]

instance Latex Transition where
    latexify Transition { system, before, after } = unwords
        [ latexify before, system, latexify after ]

instance Latex Conf where
    latexify (Conf c) =
        "\\conf{" ++ (intercalate " \\confsep " $ map latexifySl c) ++ "}"
            where latexifySl s = unwords $ map latexify s

instance Latex SyntaxElem where
    latexify (Syntax s)  = "\\syntax{" ++ s ++ "}"
    latexify (Var ve)    = latexify ve
    latexify (SubElem s) = unwords $ map latexify s

instance Latex Expr where
    latexify (EVar ve)     = latexify ve
    latexify (ECall e p)   = latexify e ++ "(" ++ (intercalate ",\\ " $ map latexify p) ++ ")"
    latexify (EEq e1 e2)   = latexify e1 ++ " = " ++ latexify e2
    latexify (EInEq e1 e2) = latexify e1 ++ " \not= " ++ latexify e2

instance Latex VariableExpr where
    latexify (VRef v)         = latexify v
    latexify (VBind ve e1 e2) = latexify ve ++ "[" ++ latexify e1 ++ "↦" ++ latexify e2 ++ "]"

instance Latex Variable where
    latexify Variable { typeName, varName, marks, literal = False, unused } = case typeName of
        Nothing -> varName ++ replicate marks '\''
        Just t -> t ++ "_{" ++ varName ++ "}" ++ replicate marks '\''
    latexify Variable { typeName, varName, marks, literal = True, unused } =
        "\\syntax{" ++ varName ++ "}"

