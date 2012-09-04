import "syntax.wisp"
import "ack.wisp"

"Quick tests of asserts / fails"

assert true


assert
	fails
		assert false

assert
	not (fails 3)

"Quick test that the syntax.wisp stuff works"

; this is a comment

assert
	== 3
		, 3


"Time to test foldLeft"

let l
	list 1 (+ 1 1) 3 4

let l2
	fold-left l 0 +

assert
	== l2 10


let add
	vau e a
		do
			let f (eval e (nth a 0))
			let s (eval e (nth a 1))
			+ f s

let l3
	fold-left l 0
		vau e a
			do
				let f (eval e (nth a 0))
				let s (eval e (nth a 1))
				+ f s

assert
	== l3 10

assert
	== 125
		ack 3 4

"...and ackermann function"

"All tests passed"

