trace "syntax.wisp loaded"

let ;
	lambda e a 0

let ,
	lambda e a (eval e (nth a 0))

