import "let.wisp"

#dict-get
	__do-let
		((#vau e a e))
		__quote
			((y (#num-add 10 15)) ("z" (#num-add 20 43) ("rabbit" (#num-add 33 444))))
	__quote y
