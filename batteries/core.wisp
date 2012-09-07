let ;
	#vau e a ()

let ,
	#vau e a (#eval e (#vect-nth a 0))

let fn
	#vau e a
		#do
			let arg-symbols (#vect-nth a 0)
			let body (#vect-nth a 1)
			#vau e2 a2
				#do
					let fold-result
						#vect-fold-left arg-symbols (#vect-make e 0)
							#vau fe fa
								#do
									let state (#eval fe (#vect-nth fa 0))
									let new-arg (#vect-nth fa 1)
									let old-env (#vect-nth state 0)
									let count (#vect-nth state 1)
									#vect-make
										#dict-insert old-env new-arg (#eval e2 (#vect-nth a2 count))
										#num-add count 1
					let built-env (#vect-nth fold-result 0)
					#eval built-env body

let inc
	fn (a)
		#num-add a 1

let dec
	fn (a)
		#num-sub a 1

dec 4
