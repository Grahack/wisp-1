import "core.wisp"
import "test.wisp"


must-fail dasdf


filter
	vect-make 1 2 3 4 5 2 3 7
	fn (n)
		#num-gt n 3


let x
	fn (& varargs)
		#trace varargs


let r 4

nest
	if (#num-eq r 44) 10
	if (#num-eq r 55) 20
	if (#num-gt 4 r) 40
	50

!= 4 "chicken"

; apply + (vect-make 1 2 3)

!= "cat" "cat" "cadt"


