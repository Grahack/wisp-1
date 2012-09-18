import
	"env-add.wisp"
	"y-combinator.wisp"
	"quote.wisp"

__env-add __do-let
	__Y
		#vau f e
			#vau a2 e2
				#eval
					__quote
						#eval
							__quote
								#if (#num-eq (#vect-length let-list) 1)
									#eval (#vect-nth let-list 0) state
									#eval
										__quote
											(#eval (#vect-nth f 0) e) new-env
												#vect-slice let-list 1 (#vect-length let-list)
										__env-add new-env
											#dict-insert state (#vect-nth (#vect-nth let-list 0) 0) (#eval (#vect-nth (#vect-nth let-list 0) 1) state)
							__env-add let-list (#eval (#vect-nth a2 1) e2)
					__env-add state (#eval (#vect-nth a2 0) e2)
