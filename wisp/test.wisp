; this is a comment

#eval
	#dict-make
		#list-make
			#quote do
			#vau
				#quote args
				#quote env
				#quote
					#if #list-empty?.args
						#error "do expects a non-empty list"
						#if #list-empty?.#list-tail.args
							#eval env #list-head.args
							#trace "Multiple args: " args []
		#list-make (#quote cat) (#num-add 1 1)
	#quote
		do (#num-add 1 1) 2 3
