import
	"do-let.wisp"
	"env-add.wisp"

__env-add __let
	#vau eq aq
		#eval
			((#vau ew aw ew))
			#vect-append (#vect-append (#vect-append () __do-let) eq) (#vect-append (#vect-append () __quote) aq)
