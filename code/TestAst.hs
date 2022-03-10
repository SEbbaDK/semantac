module TestAst where

import Ast

testdomains =
    [ Domain { domain = "Integer", spec = Integer }
    , Domain { domain = "Id", spec = Identifier }
    ]

testrules =
    [ Rule
        { name = "Add"
        , base = Trans
            { system = "=>"
            , before = Conf [ Variable "a", Syntax "+", Variable "b" ]
            , after = Conf [ Variable "c" ]
            }
        , premises = [ TEquality $ Eq (EVar "c") (EOp "+" [ EVar "a", EVar "b" ]) ]
        , properties = []
        }
    ]

testast = Top testdomains testrules

