import
	"map.wisp"
	"quote.wisp"

__map (__quote ((#num-add 1 2) (#num-add 4 3))) (#vau e a (#eval e (#eval e (#vect-nth a 0))))
