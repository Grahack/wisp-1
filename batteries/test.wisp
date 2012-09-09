import "core.wisp"

let must-fail
	#vau e a
		#do
			assert (#num-eq (#vect-length a) 1)
			let first-arg (#vect-nth a 0)
			let result (#eval e (vect-make #fails first-arg))
			assert result


