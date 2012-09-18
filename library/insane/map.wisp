import
	"y-combinator.wisp"
	"env-add.wisp"
	"quote.wisp"

__env-add __map
	__Y
		#vau f e
			#vau a2 e2
				#eval
					__quote
						#if (#num-eq (#vect-length map-col) 0)
							()
							#eval
								__quote
									#vect-cons
										(#eval (#vect-nth f 0) e)
											#vect-slice map-col 1 (#vect-length map-col)
											map-func
										map-func (#vect-nth map-col 0)
								__env-add map-func
									#eval (#vect-nth a2 1) e2
					__env-add map-col
						#eval (#vect-nth a2 0) e2
