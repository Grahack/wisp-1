import "core.wisp"

let ackermann
	fn (m n)
		do
			if (#num-eq m 0)
				inc n
				if (#num-eq n 0)
					ackermann (dec m) 1
					ackermann (dec m) (ackermann m (dec n))
