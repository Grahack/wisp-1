import
	"map.wisp"
	"quote.wisp"
	"env-add.wisp"

__env-add __map-eval
	#vau a e
		__map
			__quote ((#num-add 1 2) (#num-add 4 3))
			#vau da de
				#eval
					#eval (#vect-nth a 0) e
					#eval (#vect-nth da 0) de
