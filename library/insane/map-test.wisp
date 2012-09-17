import
	"map.wisp"
	"quote.wisp"

__map (__quote ((#num-add 1 2) (#num-add 4 3))) (#vau a e (#eval (#eval (#vect-nth a 0) e) e))
