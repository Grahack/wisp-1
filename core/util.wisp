import "syntax.wisp"

"Here's some really crazy shit"


let fn
	vau e a
		do
			let arg-symbols (nth a 0)
			let body (nth a 1)
			vau e2 a2
				do
					let fold-result
						fold-left arg-symbols (list e 0)
							vau fe fa
								do
									let state (eval fe (nth fa 0))
									let new-arg (nth fa 1)
									let old-env (nth state 0)
									let count (nth state 1)
									list
										dict-insert old-env new-arg (eval e2 (nth a2 count))
										+ count 1
					let built-env (nth fold-result 0)
					eval built-env body



