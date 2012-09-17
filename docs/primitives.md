Wisp Primitives
---------------

These are the heart-and-soul of wisp. These primitives take in their arguments unevaluated, and do magical things with them, based on the evaluation rules described here. Unless you're working on a wisp implementation, or developing the standard library (or a replacement for it) you should never actually use these primitives directly. That's why they start with a <code>#</code> to make them a bit uglier to prevent use. All the high-level functionality is built on-top of these primitives.

In addition to these primitives, there's a lot of "built-in" functions to "hook-into" the underlying system (e.g. add two numbers) or provide the basic datastructures.

<table>
	<tr>
		<th>#eval</th>
		<td>form env</td>
		<td>
			<p>This is actually a strict function. That is, both arguments are first immediately evaluted in the context of the caller. The first argument <code>form</code> can be any time, while the second argument must be a <code>Dict</code>. <code>#eval</code> will then evaluate <code>form</code> in the context of <code>env</code> and return the result.</p>

			<p>Occasionally it's desirable for <code>#eval</code> to not act as a strict function. This is very easy to do, by using a quote function (trivially built with a <code>#vau</code>).</p>

			<p>Unlike most lisps, using <code>#eval</code> (or rather, nicer wrapper of it) is not poor practise, and almost always can be eliminated with powerful static analysis, and partial evaluation. And due to wisps fexpr and static scoping, it allows to making composible and safer "primitives" than something like macros.</p>
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
		<td>args env body</td>
		<td>
			<p>This little dude, a <a href="https://en.wikipedia.org/wiki/Fexpr">fexpr</a>, is the powerhouse behind wisp -- and the reason wisp has so few primitives. Everything else is just implemented in terms of <code>#vau</code>! Intuitively, <code>#vau</code> creates an anonymous-function / closure. However, it receives its arguments <strong>unevaluated</strong>. This means, if it's called with an argument of <code>(+ 20 30)</code> it wouldn't receive <code>50</code> but <code>(+ 20 30)</code> (which it, itself could choose to evaluate).</p>

			<p>This by itself isn't enough, because we want to know the context in which we were called. Like what does <code>+</code> mean? Or maybe the argument list has some local variables etc. So that's why an <code>#vau</code> is also given the environment (a <code>Dict</code>) of the caller. Given these two things, its trivial to emulate a strict-function (just call <code>eval</code> with the callers-environment and the argument), and since the arguments aren't being evaluted -- it makes it easy to create DSLs, emulate (composibile!) macros -- or just in general, see how values are created (super-reflection style!).</p>

<pre><code>#vau argument-list caller-env
	do
		#trace "Called from a place with an env of: " caller-env
		#trace "Called with: " (#vect-length argument-list) " args"
		#trace "first (unevaluated) arg: " (#vect-nth argument-list 0)
		#trace "first (evaluated) arg:"
			eval (#vect-nth 0 argument-list) caller-env
		43</code></pre>

		<p>The name <code>vau</code> was borrowed (without permission, and permanently ;D) from the <a href="https://en.wikipedia.org/wiki/Kernel_(programming_language)">kernel programming language</a></p>

		</td>
	</tr>
</table>
