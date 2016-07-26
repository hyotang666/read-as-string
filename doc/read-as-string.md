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
