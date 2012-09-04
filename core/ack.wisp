import "util.wisp"

let ack
	fn (m n)
		do
			if (== m 0)
				+ n 1
				if (== n 0)
					ack (- m 1) 1
					ack (- m 1) (ack m (- n 1))

