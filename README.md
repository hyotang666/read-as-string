# READ-AS-STRING 2.2.9
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
(let ((sexp "(CAR (liSt :a 45.))"))
  (print (read-from-string sexp))
  (with-input-from-string (s sexp)
    (print (read-as-string s))))
(CAR (LIST :A 45))    ; <--- Output of CL:READ-FROM-STRING.
"(CAR (liSt :a 45.))" ; <--- Output of READ-AS-STRING.
```
Usually `(read-from-string (read-as-string stream))` is same with `(read stream)`.

## Exception
### Comment
Read-as-string does not discard comment.

```lisp
(with-input-from-string (s "; line comment")
  (read-as-string s))
"; line comment"
```

### Read time condition.
Read-as-string does not discard any read time condition.

```lisp
(with-input-from-string (s "#+() test")
  (read-as-string s))
"#+() test"
```

## From developer

### Product's goal
Eliminate bugs.
### License
MIT

### Tested with
* SBCL/2.1.7
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1
* Allegro/10.1
* CMUCL/21D
* ABCL/1.8.0
