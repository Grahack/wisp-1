let ;
	#vau e a ()

; Now ; is a pseudo comment


; The builtin if is pretty solid, let's just use it
let if #if

let eval #eval

let assert
	#vau e a
		if (#num-eq (#vect-length a) 0)
			#error "Assert must be given at least one argument"
			if (#num-gt (#vect-length a) 2)
				#error "Assert must not be given more than two arguments (assertion and message)"
				if (#eval e (#vect-nth a 0)) ()
					#error
						if (#num-eq (#vect-length a) 2)
							#eval e (#vect-nth a 1)
							, "Assert fail (no msg provided)"

let ,
	#vau e a
		do
			assert (#num-eq (#vect-length a) 1) ", only expects a single argument"
			#eval e (#vect-nth a 0)

let empty?
	fn (vec)
		#do
			assert (#type-eq (#type-of vec) #Vect) "empty? expected a vector"
			#num-eq (#vect-length vec) 0

; Get the first element of a non-empty vector
let head
	#vau e a
		do
			assert (#num-eq (#vect-length a) 1)
			let vec (eval e (#vect-nth a 0))
			assert (#num-gte (#vect-length vec) 0)
			#vect-nth vec 0

; Get all, but the first element of a non-empty vector
let tail
	#vau e a
		do
			assert (#num-eq (#vect-length a) 1)
			let vec (eval e (#vect-nth a 0))
			let size (#vect-length vec)
			assert (#num-gte size 0)
			#vect-slice vec 1 size

; Get the last element of a non-empty vector
let last
	#vau e a
		do
			assert (#num-eq (#vect-length a) 1)
			let vec (eval e (#vect-nth a 0))
			let size (#vect-length vec)
			assert (#num-gte size 0)
			#vect-nth vec (#num-sub size 1)

; Get all, but the last element of a non-empty vector
let init
	#vau e a
		do
			assert (#num-eq (#vect-length a) 1)
			let vec (eval e (#vect-nth a 0))
			let size (#vect-length vec)
			assert (#num-gte size 0)
			#vect-slice vec 0 (#num-sub size 1)

; TODO switch argument order
let drop-first-n
	#vau e a
		#do
			assert (#num-eq (#vect-length a) 2)
			let vec (eval e (#vect-nth a 0))
			let amnt (eval e (#vect-nth a 1))
			let size (#vect-length vec)
			assert (#num-gte size amnt)
			#vect-slice vec amnt size


let drop-last-n
	fn (n vec)
		#do
			let size (#vect-length vec)
			assert (#num-gte size n) "Can't get drop-last-n of something smaller than n"
			#vect-slice vec 0 (#num-sub size n)

let last-n
	fn (n vec)
		#do
			let size (#vect-length vec)
			assert (#num-gte size n) "Can't get last-n of something smaller than n"
			#vect-slice vec (#num-sub size n) size

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

let not #bool-not

let push #vect-append

let foldl1
	fn (f vec)
		#do
			assert (not (empty? vec))
			foldl f (head vec) (tail vec)

let foldr
	fn (f state vec)
		#if (#num-eq (#vect-length vec) 0)
			, state
			foldr f (f state (last vec)) (init vec)

let foldr1
	fn (f vec)
		#do
			assert (not (empty? vec)) "foldr1 expects a non-empty vector"
			foldr f (last vec) (init vec)

let reduce #vect-reduce

let +
	fn (& args)
		reduce #num-add args

let *
	fn (& args)
		reduce #num-mult args

let quote
	#vau e a
		#do
			assert (#num-eq (#vect-length a) 1) "Quote expects a single argument"
			#vect-nth a 0

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
									let encounted-vargs (#vect-nth state 2)
									if (#bool-eq encounted-vargs true)
										#do
											let map-eval
												#vau eme ame
													#do
														assert (#num-eq (#vect-length ame) 1)
														let vec (eval eme (#vect-nth ame 0))
														if (#num-eq (#vect-length vec) 0)
															, ()
															#vect-append (map-eval (init vec)) (eval e2 (last vec))
											vect-make
												#dict-insert old-env new-arg (map-eval (drop-first-n a2 count))
												#num-add count 1
												, true
										if (#sym-eq new-arg (quote _))
											vect-make old-env (#num-add count 1) false
											if (#sym-eq new-arg (quote &))
												vect-make old-env count true
												vect-make
													#dict-insert old-env new-arg (eval e2 (#vect-nth a2 count))
													#num-add count 1
													, false
							vect-make e 0 false
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



; for chained if statements, mainly

let nest
	#vau e a
		eval e
			foldr1 (fn (x y) (push y x)) a

let < #num-lt

let apply
	#vau e a
		#do
			assert (#num-eq (#vect-length a) 2)
			let f (eval e (#vect-nth a 0))
			let vec (eval e (#vect-nth a 1))
			eval e (#vect-cons vec f)

; TODO add check about dropping too much in a step
let for-all-slide
	fn (f window step vec)
		if (< (#vect-length vec) window)
			, true
			#do
				let elems (last-n window vec)
				if (apply f elems)
					for-all-slide f window step (drop-last-n step vec) 
					, false

let __binary-eq
	fn (a b)
		nest
			if (not (#type-eq (#type-of a) (#type-of b)))
				, false
			if (#type-eq (#type-of a) #Bool)
				#bool-eq a b
			if (#type-eq (#type-of a) #Dict)
				#error "Dict comparison not yet done"
			if (#type-eq (#type-of a) #Num)
				#num-eq a b
			if (#type-eq (#type-of a) #Str)
				#str-eq a b
			if (#type-eq (#type-of a) #Sym)
				#symb-eq a b
			if (#type-eq (#type-of a) #Type)
				#type-eq a b
			if (#type-of (#type-of a) #Vect)
				#error "Vect comparison not yet done"
			#error "Unknown type in comparison"

let ==
	fn (& args)
		#do
			assert (#num-gte (#vect-length args) 2) "== needs at least two arguments"
			for-all-slide __binary-eq 2 1 args

let !=
	fn (& args)
		#do
			assert (#num-gte (#vect-length args) 2) "!= needs at least two arguments"
			not (apply == args)
