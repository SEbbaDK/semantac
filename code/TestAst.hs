module TestAst where

import           Ast
import           Data.Sequence ((|>))

testDomains :: [Domain]
testDomains =
    [ Domain { domain = "Int", variable = "n", spec = Integer }
    , Domain { domain = "Id", variable = "x", spec = Identifier }
    ]

testSystems :: [System]
testSystems =
    [ System { arrow = "=>", initial = Custom "n", final = Custom "n" }
    ]

testRules :: [Rule]
testRules =
    [ Rule
        { name = "Add"
        , base = Trans
            { system = "=>"
            , before = Tup [ var "a", syn "+", var "b" ]
            , after = Tup [ var "c" ]
            }
        , premises = [ TEquality $ Eq (EVar "c") (EOp "+" [ EVar "a", EVar "b" ]) ]
        , properties = []
        }
    ]
    where
        var = Variable
        syn = Syntax

testAst :: Top
testAst = Top testDomains testSystems testRules

