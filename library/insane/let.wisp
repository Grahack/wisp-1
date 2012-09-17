import
	"do-let.wisp"
	"env-add.wisp"

__env-add __let
	#vau aq eq
		#eval
			#vect-append (#vect-append (#vect-append () __do-let) eq) (#vect-append (#vect-append () __quote) aq)
			((#vau aw ew ew))
