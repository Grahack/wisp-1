import "syntax.wisp"

"In terms of vau, let's write fn. Where fn is a strict function"
"and has a fixed arity"

let fn
	vau e a
		do
			let arg-symbols (nth a 0)
			let body (nth a 1)
			vau e2 a2
				do
					let fold-result
						fold-left arg-symbols (vect e 0)
							vau fe fa
								do
									let state (eval fe (nth fa 0))
									let new-arg (nth fa 1)
									let old-env (nth state 0)
									let count (nth state 1)
									vect
										dict-insert old-env new-arg (eval e2 (nth a2 count))
										num-add count 1
					let built-env (nth fold-result 0)
					eval built-env body

let inc
	fn (a)
		num-add a 1

let dec
	fn (a)
		num-sub a 1
