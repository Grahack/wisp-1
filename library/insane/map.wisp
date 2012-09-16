import
	"y-combinator.wisp"
	"env-add.wisp"

__env-add __map
	__Y
		#vau e f
			#vau e2 a2
				#eval
					__env-add map-col (#eval e2 (#vect-nth a2 0))
					__quote
						#if (#num-eq (#vect-length map-col) 0)
							()
							#eval
								__env-add map-func (#eval e2 (#vect-nth a2 1))
								__quote
									#vect-cons
										(#eval e (#vect-nth f 0)) (#vect-slice map-col 1 (#vect-length map-col)) map-func
										map-func (#vect-nth map-col 0)
