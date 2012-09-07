import "core.wisp"

let assert
	fn (cond)
		#if cond #bool-true (#error "Assertion failed")

