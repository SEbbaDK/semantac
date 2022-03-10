testdomains = [
	Domain { domain = "Integer", spec = Integer },
	Domain { domain = "Id", spec = Identifier }
]

testrules = [
	Rule {
		name = "Add",
		base = Trans {
			system = "=>",
			before = [ Variable "a", Syntax "+", Variable "b" ],
			after = [ Variable "c" ]
		},
		premises = [ TEquality $ Eq (EVar "c") (Op "+" [ EVar "a", Evar "b" ]) ],
		properties = []
	}
]

testast = Top testdomains testrules

