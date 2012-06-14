Syntax
------
This is a comprehensive tutorial to Wisps syntax. After reading this guide, you will be an expert on the syntax, and no statement will ever be ambigious.

Atom
====
An atom is either a: Number, String, Symbol or a List

Number
===============
Numbers are integer values. They can not contain a decimal place or commas

Valid Numbers: `1237` `0` `-123`

Invalid: `423.7` `1,000`

String
=======
Strings start with a quote `"` and end with a quote `"`. They may contain whitespace, but may not span more than one line. There are the following defined escape sequences: `\n` (newline) `\"` (a double quote) `\t` (tab)

Valid Strings: `"ch- icken"` `"\"soup?\" he asked"` `"man\ndog`

Invalid Strings: `ch- icken` `""soup?" he asked"` `"undefined escape \e" `


Symbol
=======
Everything is a symbol, provided it doesn't contain whitespace or control characters (space, tabs, endline etc.). Additionally it must not contain a quote, or start with something that would a number.

 Valid Symbols: `dog` `cat` `+` `=`  `<3` `true` `-lol` `@#$#^#$/*`
 Invalid Symbols: `has space` `4chan` `-2lol` `sup"dog`
 
List
====
A list is when ever there is *two or more* atoms together. Anytime two-or-more atoms on a single line, that's a list.

Valid lists:
```
1 2 3 4
+ cat dog
chicken soup 332 "lol"
``` 

Putting everything on a single line, is both awkward and limiting. So you are allowed to use multiple lines, provided that you you ident a *single extra* tab. For instance:

```1 2 3 4 5```

can also be written

```
1
	2
	3
	4
	5
```


Using our earlier rule /anytime two-or-more atoms on a single line, that's a list/ allow us to easily create lists of lists:

```
q
	a c
	44
	1
		"chicken" 343
```

This is equivilent to `[q, [a, c], 44, [1, ["chicken", 343]]]` in a more traditional syntax.



The End
=======

That's it folks! You know all the syntax!.


Some Notes
=========

* Indentation is done with tabs. You must start at 0 tabs, the next level must have 1 tab, the next level is 2 tabs. The reader is very strict about this. Mixing tabs and spaces at the start of a line is absolutely not allowed.

* Blank lines are ignored.

* With this syntax, it's not possible to represent an empty list, a list with one element or a list that starts with a list. In practise, this is trivially solvable with some builtin functions (think of they as more powerful versions of macros), and symbol that resolves to an empty-list. However, in the future I might add () to manually make a list, to avoid these theoretical limitaitons.

* There's no comments. But you can easily make/use a function that throws away its arguments. In practice it becomes very easy to use:

```
// the symbol "//" is my comment function, which throws aways its arguments
// as a bonus, i can even use multi line comments for free
	this is part of the above list, neat huh?
// the only thing to becareful of is having an unterminated (double) quote, as that will screw the parser	
```

* When using the repl, if you end a line with `\` the repl will automatically add a new identation for you, and allow you to continue entering your list. However, the repl strips out the `\` and it is never even given to wisp, nor part of its syntax.

* When a file is read, it gives a List of Atoms.







 

