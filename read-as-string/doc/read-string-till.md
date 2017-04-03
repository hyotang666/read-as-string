# [Function] READ-STRING-TILL

## Syntax:

(READ-STRING-TILL predicate &optional input-stream eof-error-p eof-value consume) => STRING

## Arguments and Values:

predicate := (FUNCTION(T)GENERALIZED-BOOLEAN)

input-stream := INPUT-STREAM The deafult is `*STANDARD-INPUT*`

eof-error-p := BOOLEAN The default is T.

eof-value := ANY-LISP-OBJECT The default is NIL.

consume := BOOLEAN The default is NIL.

## Description:

Read characters from INPUT-STREAM untill met character does not satisfies PREDICATE.
Then coerce collected characters into string.
When CONSUME is true delimiter character is consumed, otherwise such character is UNREADed.

EOF-ERROR-P and EOF-VALUE is completely same with READ, READ-LINE, READ-CHAR.

## Example:
```lisp
(with-input-from-string(*standard-input* "aaabc")
  (print(read-string-till(lambda(c)(char= #\b c))))
  (read-char))
=> 
"aaa" ; print side effect.
#\b
(with-input-from-string(in "aaabc")
  (print(read-string-till(lambda(c)(char= #\b c)) in T T T))
  (read-char))
=>
"aaab" ; print side effect.
#\c
```
## See Also:
