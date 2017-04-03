# [Function] READ-STRING-TILL

## Syntax:

(READ-STRING-TILL pred &optional (*standard-input* *standard-input*) (eof-error-p t) (eof-value nil) (consume nil)) => result

## Arguments and Values:

pred :=

*standard-input* :=

eof-error-p :=

eof-value :=

consume :=

result := 

## Description:

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

## Affected by:

`**`
`*READ-EVAL*`
`*STANDARD-OUTPUT*`
`*READ-BASE*`
`++`
`*FEATURES*`
`*PRINT-MISER-WIDTH*`
`LEAST-POSITIVE-LONG-FLOAT`
`*TRACE-OUTPUT*`
`*COMPILE-VERBOSE*`
`MOST-POSITIVE-LONG-FLOAT`
`*DEFAULT-PATHNAME-DEFAULTS*`
`*PRINT-READABLY*`
`*QUERY-IO*`
`*PRINT-GENSYM*`
`*MACROEXPAND-HOOK*`
`*LOAD-PRINT*`
`PI`
`*STANDARD-INPUT*`
`*PRINT-LEVEL*`
`*TERMINAL-IO*`
`*LOAD-TRUENAME*`
`*COMPILE-PRINT*`
`*PRINT-PRETTY*`
`*LOAD-VERBOSE*`
`*READTABLE*`
`*PRINT-RADIX*`
`MOST-NEGATIVE-LONG-FLOAT`
`*RANDOM-STATE*`
`*COMPILE-FILE-PATHNAME*`
`*DEBUGGER-HOOK*`
`*ERROR-OUTPUT*`
`***`
`*PRINT-LENGTH*`
`LEAST-NEGATIVE-LONG-FLOAT`
`*DEBUG-IO*`
`*COMPILE-FILE-TRUENAME*`
`LONG-FLOAT-NEGATIVE-EPSILON`
`*GENSYM-COUNTER*`
`*BREAK-ON-SIGNALS*`
`*PACKAGE*`
`*MODULES*`
`*PRINT-CASE*`
`*PRINT-RIGHT-MARGIN*`
`*READ-SUPPRESS*`
`*LOAD-PATHNAME*`
`LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT`
`*PRINT-ESCAPE*`
`*PRINT-BASE*`
`LONG-FLOAT-EPSILON`
`*PRINT-ARRAY*`
`*READ-DEFAULT-FLOAT-FORMAT*`
`*PRINT-PPRINT-DISPATCH*`
`//`
`*PRINT-CIRCLE*`
`LEAST-POSITIVE-NORMALIZED-LONG-FLOAT`
`/`
`-`
`+`
`*`
`///`
`*PRINT-LINES*`
`+++`

## Example:

## Side-Effects:

## Notes:

## Exceptional-Situations:

## See-Also:

COMMENTP
READ-AS-STRING
READ-STRING-TILL
SPACE-CHAR-P
TERMINAL-CHAR-P
