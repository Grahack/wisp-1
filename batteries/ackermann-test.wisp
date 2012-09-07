import "ackermann.wisp"
import "test.wisp"

assert
	#num-eq
		ackermann 3 4
		, 125

"Ackermann function has been successfully tested"
