# READ-AS-STRING 2.1.3
## What is this?
Reading S-Expression string from stream.

## Current lisp world
Unlike UNIX, Lisp has READ.
Lisp programmer does not make effort to make S-Expression parser (UNIX has awk though).

## Issues
Sometime we need to analyze S-Expression statically.
In this context, 'static' means does not construct lisp object but string.
But lisp does not have such feature.

## Proposal
READ-AS-STRING do it.

## Usage
```lisp
(let((sexp "(CAR (liSt :a 45.))"))
  (print(read-from-string sexp))
  (with-input-from-string(s sexp)
    (print (read-as-string s))))
(CAR (LIST :A 45))    ; <--- Output of CL:READ-FROM-STRING.
"(CAR (liSt :a 45.))" ; <--- Output of READ-AS-STRING.
```
Usually `(read-from-string (read-as-string stream))` is same with `(read stream)`.

## From developer

### Product's goal
Eliminate bugs.
### License
MIT

### Tested with
* SBCL/2.0.0
* CCL/1.11.5
* ECL/16.1.3
* CLISP/2.49
