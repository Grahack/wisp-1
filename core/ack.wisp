import "util.wisp"

let ack
	fn (m n)
		do
			if (num-eq m 0)
				inc n
				if (num-eq n 0)
					ack (dec m) 1
					ack (dec m) (ack m (dec n))

