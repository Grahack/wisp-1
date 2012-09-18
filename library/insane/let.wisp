import
	"do-let.wisp"
	"env-add.wisp"

__env-add __let
	#vau a e
		#eval
			#vect-append
				#vect-append (#vect-append () __do-let) e
				#vect-append (#vect-append () __quote) a
			((#vau _ de de))
