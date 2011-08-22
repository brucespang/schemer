This is a half decent scheme-like programming language. *Please* do not use it for anything important.

Getting Started
==============
Everything you need is contained in `lisp.rb`. To run the REPL, run `ruby lisp.rb`. To run a one off command, run `ruby lisp.rb "<command>"`. Multiple one off commands can be chained together (e.g. `ruby lisp.rb "(define a 1)" "(+ a 2)"` will return 3). To load and run a file, run `ruby lisp.rb <filename>`. There are some unit tests, implemented in this language. They are not close to comprehensive. To run them, run `ruby lisp.rb "(run-tests)"`.

Things that are implemented:
=========================
* `define`, for both normal variables and functions
* `if`, and `cond`
* `cons`, `car`, `cdr`, and `cadr`
* `quote`
* `eq?`, `true?`, `false?`, `not`
* math functions (`+`, `-`, `*`, `/`)
* A REPL

Notable things that aren't implemented:
=============================
* `let`
* quoting with `'(<quoted list>)`
* `set!` and its derivatives
* Strings
* Proper variable errors
* Decent backtraces
* Multi-line entry in the REPL.
* Loading of external files.
* Readable printing of cons pairs.
* A proper lexical analyzer.