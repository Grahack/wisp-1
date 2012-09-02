import "syntax.wisp"

let y 45

let ack
	lambda e a
		do
			let m (eval e (nth a 0))
			let n (eval e (nth a 1))
			if (== m 0)
				+ n 1
				if (== n 0)
					ack
						- m 1
						, 1
					ack
						- m 1
						ack m
							- n 1

ack 2 180



