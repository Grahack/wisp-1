Syntax
------
This is a comprehensive tutorial to Wisps syntax. After reading this guide, you will be an expert on the syntax, and no statement will ever be ambigious.

Wisp is has a very strict reader, it will not accept even slightly malformed input. This keeps it simple, and avoids any confusion. For instance, indentation must be done with tabs. Atom seperation must be done with spaces. New lines must be `\n`. Nothing else will the reader accept.



File
====

A file is made up of list of lines. A line may be blank, or may contain a *single* atom (but an atom itself might be a list). The result of reading a file, is a list of atoms. 


Atom
====
An atom is either a: Number, Char, Symbol or a List

Number
======
Numbers are integer values. They (currently) can not contain a decimal place or commas

Valid Numbers: `1237` `0` `-123`

Invalid: `423.7` `1,000`

Char
======
A character is prefixed by using a single backquote `\`. The subsequent character is interpretted as the character.

Escape Characters: `\space` `\tab` `\newline` `\semicolon` `\backslash`

Valid Characters: `'a` `'5` `'\t`

Invalid: `'ab` `' `


Symbol
=======
Everything is a symbol, provided it doesn't contain whitespace or control characters (space, tabs, endline etc.). Additionally it must not contain a quote, or start with something that would a number.

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

Convenience Lists:
========

Often you would need to write `(#VectMake 1 2 3)` to make something that evaluates to a list of three elements. A convience syntax for this is: `[1 2 3]`. The reader reads it identically to as if you wrote `(#VectMake 1 2 3)`

And likewise, you would find yourself writing things like `['h 'e 'l 'l 'o]` very often. So if your list is a string (or rather, a list of characters, you can use the convience quoted string syntax. Just like the `[]` it reads it with the #VectMake prefix. So `"cowboy"` is read as: `(#VectMake 'c 'o 'w 'b 'o 'y)`

Strings may not contain: end of lines, or double-quotes. `\` is used as an escape-sequence:

Escape sequence: `\\` (backslash) `\n` (newline) `\"` (quote)

Valid Strings: `"ch- icken"` `"\"soup?\" he asked"` `"man\ndog"`

Invalid Strings: `ch- icken` `""soup?" he asked"` `"undefined escape \e"`


Comments:
=======
Comments start with `;` and continue to the end of the line. It is not valid to use `;` inside of strings, symbols or characters. The char you need to use is`\semicolon`

Note
=========

* Indentation is done with tabs. You must start at 0 tabs, the next level must have 1 tab, the next level is 2 tabs. The reader is very strict about this. Mixing tabs and spaces at the start of a line is absolutely not allowed.

* Blank lines are ignored.

* When a file is read, it gives a List of Atoms.

