import
	"map.wisp"
	"quote.wisp"
	"env-add.wisp"

__env-add __map-eval
	#vau e a
		__map
			__quote ((#num-add 1 2) (#num-add 4 3))
			#vau de da
				#eval
					#eval e (#vect-nth a 0)
					#eval de (#vect-nth da 0)
