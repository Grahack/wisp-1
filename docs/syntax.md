Syntax
------
This is a comprehensive tutorial to Wisps syntax. After reading this guide, you will be an expert on the syntax, and no statement will ever be ambigious.

Wisp is has a very strict reader, it will not accept even slightly malformed input. This keeps it simple, and avoids any confusion. For instance, indentation must be done with tabs. Atom seperation must be done with spaces. New lines must be `\n`. Nothing else will the reader accept.



File
====

A file is made up of list of lines. A line may be blank, or may contain a *single* atom (but an atom itself might be a list). The result of reading a file, is a list of atoms. 


Atom
====
An atom is either a: Number, String, Symbol or a List

Number
======
Numbers are integer values. They (currently) can not contain a decimal place or commas

Valid Numbers: `1237` `0` `-123`

Invalid: `423.7` `1,000` *(Note: in the future, probably both of these will be valid)*

String
======
Strings start with a quote `"` and end with a quote `"`. They may contain whitespace, but may not span more than a line. They support c-style escape characters e.g. `\n` for newline. *(Note: escape characters not yet implemented)*

Valid Strings: `"ch- icken"` `"\"soup?\" he asked"` `"man\ndog`

Invalid Strings: `ch- icken` `""soup?" he asked"` `"undefined escape \e" `


Symbol
=======
Everything is a symbol, provided it doesn't contain whitespace or control characters (space, tabs, endline etc.). Additionally it must not contain a quote, or start with something that would a number. *(Note: in the future escape characters will probably be allowed inside a symbol)*

Valid Symbols: `dog` `cat` `+` `=`  `<3` `true` `-lol` `@#$#^#$/*`

Invalid Symbols: `has space` `4chan` `-2lol` `sup"dog`
 
List
====
A list starts with a `(` contains zero or more atoms, and ends with a `)`. *This must be on a single line*

Valid lists: `(1 "chicken" dog)` `()` `(+ (***) (sup) ())` 

Using the above syntax, you have absolutely everything you need. However, as you probably guessed by the project name, and extremely strict rules extremely strict rules (no new lines in side a list, and one atom per line) there is a much nicer way to create lists.

Implicit Lists
==============
These are ordinary lists, except you don't need to manually add the parenthesis. Let's try an example: `332 chicken`

The wisp reader sees this line with two atoms, which would be a violate the "one atom per line" statement we said before. This is because the parser knows you must have meant for this to be a list, with two atoms. So it's exactly the same as if you had written: `(332 chicken)`

This is cool, but doesn't get us very far. The real trick is if the next (non empty) lines are idented *one* more tab level -- they belong to the original line.

e.g.

```
first	second
	third

	fourth
```

Is the same as: `first second third fourth`, which in turn is the same as: `(first second third fourth)`

Note that this identation level rule applies to the idented line too! So:

```
add
	multiply 1 2
	subtract
		add 1 2
```

means: `(add (multiple 1 2) (subtract (add 1 2)))`


And that's all there is to it! Check your understanding by figuring out what this is equivilent to with explicit list syntax:

```
q
	a c
		foo

	44
		1
		"chicken" 343
```


If you got `(q (a c foo) (44 1 ("chicken" 343)))` then congratulate yourself, you're now an expert!



Note
=========

* Indentation is done with tabs. You must start at 0 tabs, the next level must have 1 tab, the next level is 2 tabs. The reader is very strict about this. Mixing tabs and spaces at the start of a line is absolutely not allowed.

* Blank lines are ignored.

* There's no comments, but in practise this isn't an issue as you can easily just filter out all lists that start with the `//` symbol (or what ever your prefer). e.g.

```
// the symbol "//" is my comment function, which throws aways its arguments
// as a bonus, i can even use multi line comments for free
	this is part of the above list, neat huh?
// the only thing to becareful of is having an unterminated (double) quote, as that will screw the parser	
```

* When using the repl, if you end a line with `\` the repl will automatically add a new identation for you, and allow you to continue entering your list. However, the repl strips out the `\` and it is never even given to wisp, nor part of its syntax.

* When a file is read, it gives a List of Atoms.

