+ 1 2 3 y
let y 34

let ,
	lambda a (nth a 0)

let ack
	lambda a
		do
			let m (nth a 0)
			let n (nth a 1)
			if (== m 0)
				+ n 1
				if (== n 0)
					ack (- m 1) 1
					ack
						- m 1
						ack m (- n 1)

ack 3 6


