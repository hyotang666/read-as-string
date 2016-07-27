# READ-AS-STRING - Reader which returns S-Expression string.

* Current lisp world
Unlike UNIX, Lisp has READ.
Lisp programmer does not make effort to make S-Expression parser (UNIX has awk though).

* Issues
Sometime we need to analyze S-Expression statically.
In this context, 'static' means does not construct lisp object but string.
But lisp does not have such feature.

* Propose
READ-AS-STRING do it.

## Usage
```lisp
(let((sexp "(CAR (liSt :a 45.))"))
  (print(read-from-string sexp))
  (with-input-from-string(s sexp)
    (print (read-as-string s))))
(CAR (LIST :A 45))
"(CAR (liSt :a 45.))"
```
Usually `(read-from-string (read-as-string stream))` is same with `(read stream)`.

## From developer

* Product's goal - eliminate bugs.
* License - MIT
* Developped with - CLISP
* Tested with - none

