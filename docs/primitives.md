Wisp Primitives
---------------

These are the heart-and-soul of wisp. These primitives take in their arguments unevaluated, and do magical things with them, based on the evaluation rules described here. Unless you're working on a wisp implementation, or developing the core library (or a replacement for it) you should never actually use these primitives directly. That's why they start with a <code>#</code> to make them a bit uglier to prevent use. All the high-level functionality is built ontop of these primitives.

In addition to these primitives, there's a lot of "built-in" functions to "hook-into" the underlying system (e.g. add two numbers) or provide the basic datastructures.

<table>
	<tr>
		<th>Builtin</th>
		<th>Arguments</th>
		<th>Description</th>
	</tr>
	<tr>
		<th>#do</th>
		<td>form*</td>
		<td>
			<p><code>#do</code> sequentially evaluates statements (in the context of the caller), and returns the result of evaluating the last statement.</p>
<pre><code>#do
	"blah"
	40
	#num-add 84 23<code></pre>
			<p>would return 107.</p>

			<p>The last statement of a <code>#do</code> ideally would be tail-call optimized, as is the case in the current scala implementation. But its yet to be seen how practical this is to guarantee when compiling to javascript.</p>

			<p>This sort of style <code>#do</code> allows is especially useful for doing outputing some debug information, or making some assertions</p>

			<p>Note, it is my intention to attempt to remove <code>#do</code> with a version built ontop of <code>#vau</code></p>
		</td>
	</tr>
	<tr>
		<th>#eval</th>
		<td>env form</td>
		<td>
			<p>The first argument <code>env</code> is immediately evaluated (in the context of the caller) which is expected to result in a <code>Dict</code>. Now the second argument <code>form</code>, is evaluated in the context of <code>env</code> (the resolved/evaluated first argument).</p>

<pre><code>#eval
	a-dict-with-x-and-y-in-it
	#num-add x y
</code></pre>

		<p>would evaluate <code>(#num-add x y)</code> in the context of <code>a-dict-with-x-and-y-in-it</code> (which presumably has x and y defined as some numbers).</p>

		<p>Unlike most lisps, using #eval (or rather, nicer wrapper of it) is not a poor practise, and almost always can be eliminated with powerful static analysis, and partial evaluation. And due to wisps fexpr and static scoping, it allows to making composible and safer "primitives" than something like macros.</p>
		</td>
	</tr>
	<tr>
		<th>#if</th>
		<td>cond true-case false-case</td>
		<td>
			<p>Actually <code>#if</code> is pretty much what you'd expect. The first argument <code>cond</code> is evaluated (in the context of the caller). If it's true, then the second argument <code>true-case</code> is evaluated (in the context of the caller) and returned. On the other hand, if <code>cond</code> was false -- only the third argument <code>false-case</code> would be evaluated (again, in the context of the caller), and returned.</p>

			<p>The current scala implementation always calls either true-case or false-case with tail-call optimization. But this might not be the case in exceptional circumstances, when compiled onto Javascript</p>
		</td>
	</tr>
	<tr>
		<th>#vau</th>
		<td>env args body</td>
		<td>
			<p>This little dude, a <a href="https://en.wikipedia.org/wiki/Fexpr">fexpr</a>, is the powerhouse behind wisp -- and the reason wisp has so few primitives. Everything else is just implemented in terms of <code>#vau</code>! Intuitively, <code>#vau</code> creates an anonymous-function / closure. However, it receives its arguments <strong>unevaluated</strong>. This means, if it's called with an argument of <code>(+ 20 30)</code> it wouldn't receive <code>50</code> but <code>(+ 20 30)</code> (which it, itself could choose to evaluate).</p>

			<p>This by itself isn't enough, because we want to know the context in which we were called. Like what does <code>+</code> mean? Or maybe the argument list has some local variables etc. So that's why an <code>#vau</code> is also given the environment (a <code>Dict</code>) of the caller. Given these two things, its trivial to emulate a strict-function (just call <code>eval</code> with the callers-environment and the argument), and since the arguments aren't being evaluted -- it makes it easy to create DSLs, emulate (composibile!) macros -- or just in general, see how values are created (super-reflection style!).</p>

<pre><code>#vau caller-env argument-list
	#do
		#trace "Called from a place with an env of: " caller-env
		#trace "Called with: " (#vect-length argument-list) " args"
		#trace "first (unevaluated) arg: " (#vect-nth 0 argument-list)
		#trace "first (evaluated) arg:"
			eval caller-env (#vect-nth 0 argument-list)
		43</code></pre>
		</td>
	</tr>
</table>
