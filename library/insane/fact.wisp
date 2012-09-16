import
	"y-combinator.wisp"
	"env-add.wisp"

__env-add fact
	__Y
		#vau e f
			#vau e2 n2
				#if (#num-eq (#eval e2 (#vect-nth n2 0)) 0)
					1
					#num-mult
						#eval e2 (#vect-nth n2 0)
						(#eval e (#vect-nth f 0))
							#num-sub (#eval e2 (#vect-nth n2 0)) 1


