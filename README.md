# READ-AS-STRING 3.0.0
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

## Readtable.

### Readers.
For efficiency, in other words, in order to avoid constructing the intermediate string repeatedly,
each reader is designed as a printer to the standard output.
Read-as-string the top-level interface captures all output and then builds a string.

If you want to extend a readtable, your reader should have API as (function * null).

### How to extend.
Read-as-string provides readtable named as-string.
To extend macro character, you can do it with an ordinary lisp manner
but to extend dispatch macro character, you need to obey our manner.
Under the hood, we need to use our own # macro character for generating the dispatch macro character notation string.
In other words, we can not use common lisp default dispatch macro character features.
In order to extend the dispatch macro character,
you should use read-as-string:set-dispatcher instead of cl:set-dispatch-macro-character.

```lisp
(let ((read-as-string:*default-readtable*
        (named-readtables:copy-named-readtable 'read-as-string:as-string)))
  (read-as-string:set-dispatcher #\! '|#!reader|)
  (read-as-string:read-as-string))
```

## From developer

### Product's goal
Eliminate bugs.
### License
MIT

### Tested with
* SBCL/2.2.5
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1
* Allegro/10.1
* CMUCL/21D
* ABCL/1.9.0
