# [Function] READ-AS-STRING

## Syntax:

(READ-AS-STRING &optional (*standard-input* *standard-input*) (eof-error-p t) (eof-value nil) (recursive-p nil)) => result

## Arguments and Values:

*standard-input* :=

eof-error-p :=

eof-value :=

recursive-p :=

result := 

## Description:

# [Function] READ-AS-STRING

## Syntax:

(READ-AS-STRING INPUT-STREAM &OPTIONAL eof-error-p eof-value recursive-p)
=> STRING

## Arguments and Values:

eof-error-p := GENERALIZED-BOOLEAN The default is T.

eof-value := ANY-LISP-OBJECT The default is NIL.

recursive-p := GENERALIZED-BOOLEAN The deafult is NIL.

## Description:

READ-AS-STRING's behavior and API is designed almost same with READ.

But there are two differences.

1. Return value is string which may be S-expression.
2. When INPUT-STREAM contains comment which on toplevel, READ-AS-STRING return string contains only such comments.

In many case `(read-from-string(read-as-string stream))` is same with (read stream).
But in above cases, result is different.

## Example:
```lisp
(with-input-from-string(in "; comment.")
  (read-from-string(read-as-string in)))
=> ERROR of type END-OF-FILE.
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
