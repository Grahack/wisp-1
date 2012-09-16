import
	"env-add.wisp"
	"y-combinator.wisp"
	"quote.wisp"

__env-add __do-let
	__Y
		#vau e f
			#vau e2 a2
				#eval
					__env-add starting-env (#eval e2 (#vect-nth a2 0))
					__quote
						#eval
							__env-add let-list (#eval e2 (#vect-nth a2 1))
							__quote
								#if (#num-eq (#vect-length let-list) 0)
									starting-env
									#eval
										__env-add new-env
											#dict-insert starting-env (#vect-nth (#vect-nth let-list 0) 0) (#eval starting-env (#vect-nth (#vect-nth let-list 0) 1))
										__quote
											(#eval e (#vect-nth f 0)) new-env
												#vect-slice let-list 1 (#vect-length let-list)
