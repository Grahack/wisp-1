import
	"y-combinator.wisp"
	"env-add.wisp"

__env-add fact
	__Y
		#vau fnc e
			#vau n2 e2
				#if (#num-eq (#eval (#vect-nth n2 0) e2) 0)
					1
					#num-mult
						#eval (#vect-nth n2 0) e2
						(#eval (#vect-nth fnc 0) e)
							#num-sub (#eval (#vect-nth n2 0) e2) 1


