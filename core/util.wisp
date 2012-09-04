import "syntax.wisp"


; fn (a b c) (+ a b c)


vau e a 


let fn
	vau e a
		do
			let argSymbols (nth 0 a)
			let body (nth 1 a)
			vau e2 a2
				eval
					fold-left argSymbols (list e 0)
						(state, newArg) =>
							do
								let old-env (nth state 0)
								let count (nth state 1)
								list
									map-insert oldE newArg
									+ count 1
					
			
			
			
			map-insert e (nth 0 args)
;		let b (nth 1 args)
;		let c (nth 2 args)
;		+ a b c








; match x
;   (fn (a) (== a 4)) 45
;   (< 53) 23
;   
