let ;
	#vau e a ()

; Now ; is a pseudo comment


; The builtin if is pretty solid, let's just use it
let if #if

let eval #eval

let assert
	#vau e a
		#do
			if (#num-eq (#vect-length a) 1) ()
				#error "Assert only expects a single argument"
			let cond (#vect-nth a 0)
			if (#eval e cond) ()
				#error "Assertion failed"

let ,
	#vau e a
		do
			assert (#num-eq (#vect-length a) 1)
			#eval e (#vect-nth a 0)

let head
	#vau e a
		do
			assert (#num-eq (#vect-length a) 1)
			let vec (eval e (#vect-nth a 0))
			assert (#num-gte (#vect-length vec) 0)
			#vect-nth vec 0

let tail
	#vau e a
		do
			assert (#num-eq (#vect-length a) 1)
			let vec (eval e (#vect-nth a 0))
			let size (#vect-length vec)
			assert (#num-gte size 0)
			#vect-slice vec 1 size

let drop
	#vau e a
		#do
			assert (#num-eq (#vect-length a) 2)
			let vec (eval e (#vect-nth a 0))
			let amnt (eval e (#vect-nth a 1))
			let size (#vect-length vec)
			assert (#num-gte size amnt)
			#vect-slice vec amnt size

let foldl
	#vau e a
		#do
			assert (#num-eq (#vect-length a) 3)
			let f (eval e (#vect-nth a 0))
			let state (eval e (#vect-nth a 1))
			let vec (eval e (#vect-nth a 2))
			if (#num-eq (#vect-length vec) 0)
				, state
				foldl f (f state (head vec)) (tail vec)

let vect-make
	#vau orig-e a
		foldl
			#vau e2 a2
				#do
					assert (#num-eq (#vect-length a2) 2)
					let astate (#eval e2 (#vect-nth a2 0))
					let n (#eval e2 (#vect-nth a2 1))
					let r (eval orig-e n)
					#vect-append astate r
			()
			, a


; Since this is soooo slow, we will cheat and use a hacked
; builtin provided by the interpretter. The real one is
; called fn-real and should be periodically tested, to make
; sure it still works ;D

let fn #fn
let fn-real
	#vau e a
		#do
			let arg-symbols (#vect-nth a 0)
			let body (#vect-nth a 1)
			#vau e2 a2
				#do
					let fold-result
						foldl
							#vau fe fa
								#do
									let state (eval fe (#vect-nth fa 0))
									let new-arg (eval fe (#vect-nth fa 1))
									let old-env (#vect-nth state 0)
									let count (#vect-nth state 1)
									if (#sym-eq new-arg (#quote _))
										vect-make old-env (#num-add count 1)
										vect-make
											#dict-insert old-env new-arg (eval e2 (#vect-nth a2 count))
											#num-add count 1
							vect-make e 0
							, arg-symbols
					let built-env (#vect-nth fold-result 0)
					eval built-env body

let inc
	fn (a)
		#num-add a 1

let dec
	fn (a)
		#num-sub a 1

; Now this is pretty interesting, what would a 'better' do, do?
let do #do

let true #bool-true
let false #bool-false

let filter
	fn (vec f)
		foldl
			fn (s x)
				if (f x)
					#vect-append s x
					, s
			, ()
			, vec

; What I want now is a convenient anonymous function
; so I can write (\ eq %1 %2)


let \
	#vau e a
		#do
			let syms
				filter a
					fn (x) (#type-eq (#type-of x) #Sym)
			#trace "Symbols are " syms

