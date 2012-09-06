import "syntax.wisp"
import "ack.wisp"

"Quick tests of asserts / fails"

assert true


assert
	fails
		assert false

assert
	not (fails 3)


let binding (vect 3 4 5)
assert
	num-eq
		nth binding 0
		, 3

let (binding-a binding-b) (vect 4 5)

assert
	num-eq binding-a 4

assert
	num-eq binding-b 5

assert
	fails
		do
			let () 444

assert
	fails
		do
			let (foo &) (vect 1 2)

; assert
	do

let (test-a (test-q test-d) test-c & test-rest) (vect 1 (vect 7 7) 2 3 4 5 6 7)


; this is a comment


"Time to test foldLeft"

let l
	vect 1 (num-add 1 1) 3 4

let l2
	fold-left l 0 num-add

assert
	num-eq l2 10


let add
	vau e a
		do
			let f (eval e (nth a 0))
			let s (eval e (nth a 1))
			num-add f s

let l3
	fold-left l 0
		vau e a
			do
				let f (eval e (nth a 0))
				let s (eval e (nth a 1))
				num-add f s

assert
	num-eq l3 10

assert
	num-eq 125
		ack 3 4

"...and ackermann function"

"All tests passed"

