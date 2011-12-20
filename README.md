This is a half decent scheme-like programming language. __Please__ do not use it for anything important.

USAGE
=====
./schemer <files> - execute files sequentially
./schemer - read from stdin

Example Code
===========
    (define (factorial n)
      (cond ((eq? n 0) 1)
            (else (* n (factorial (- n 1))))))

    (factorial 10) ;; Returns 3628800.0

Things that are implemented:
=========================
* `define`, for both variables and functions
* `if`, and `cond`
* `cons`, `car`, `cdr`, and `cadr`
* `quote`
* `eq?`, `true?`, `false?`, `not`
* math functions (`+`, `-`, `*`, `/`)

Notable things that aren't implemented:
=============================
* `let`
* Quoting with `'(<quoted list>)`
* `set!` and its derivatives
* Decent backtraces
* Loading of external files.