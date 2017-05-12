(defpackage :read-as-string.spec (:use :cl :jingoh :read-as-string))
(in-package :read-as-string.spec)
(setup :read-as-string)

(requirements-about READ-AS-STRING :test string=)

;;;; Description:
; read s-expression from specified input stream, then construct string.
#?(with-input-from-string(*standard-input* "foo")
    (read-as-string))
=> "foo"

#+syntax
(READ-AS-STRING &optional (*standard-input* *standard-input*) (eof-error-p t) (eof-value nil) (recursive-p nil)) ; => result

;;;; Arguments and Values:

; `*standard-input*` := input-stream, otherwise error.
#?(read-as-string "hoge") :signals error
,:ignore-signals warning

; eof-error-p := boolean. specify to signal error when get eof.
; The default is T.
#?(with-input-from-string(s "")
    (read-as-string s))
:signals end-of-file

#?(with-input-from-string(s "")
    (read-as-string s nil))
=> NIL
,:test eq

; eof-value := T, return value when get eof.
; The default is nil.
#?(with-input-from-string(s "")
    (read-as-string s nil :return-value))
=> :return-value

; recursive-p := boolean. internal use. 
#?(with-input-from-string(s "  foo  ")
    (read-as-string s))
=> "foo"
,:test string=
#?(with-input-from-string(s "  foo  ")
    (read-as-string s t t t))
=> "  foo"

; result := string when success, T when met eof and eof-error-p is specified nil.

;;;; Affected By:
; none

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:

;;;; Exceptional-Situations:

;;;; examples.
#?(with-input-from-string(*standard-input* "foo") ; symbol
    (read-as-string))
=> "foo" ; <--- case sensitive.

#?(with-input-from-string(*standard-input* ":foo") ; keyword
    (read-as-string))
=> ":foo"

#?(with-input-from-string(*standard-input* "foo:foo") ; package prefixed
    (read-as-string))
=> "foo:foo"

#?(with-input-from-string(*standard-input* "foo::foo") ; internal
    (read-as-string))
=> "foo::foo"

#?(with-input-from-string(*standard-input* "#:foo") ; uninterned
    (read-as-string))
=> "#:foo"

#?(with-input-from-string(*standard-input* "\\#foo") ; with escaped
    (read-as-string))
=> "\\#foo"

#?(with-input-from-string(*standard-input* "|foo|") ; with vertical bar
    (read-as-string))
=> "|foo|"

#?(with-input-from-string(*standard-input* "()") ; empty list
    (read-as-string))
=> "()"

#?(with-input-from-string(*standard-input* "(") ; unbalanced
    (read-as-string))
:signals end-of-file

#?(with-input-from-string(*standard-input* "(())") ; nested
    (read-as-string))
=> "(())"

#?(with-input-from-string(*standard-input* "(foo . bar)") ; dotted
    (read-as-string))
=> "(foo . bar)"

#?(with-input-from-string(*standard-input* "(foo ( bar . bazz))") ; nested
    (read-as-string))
=> "(foo ( bar . bazz))"

#?(with-input-from-string(*standard-input* "#(0 1 2 3)") ; vector
    (read-as-string))
=> "#(0 1 2 3)"

#?(with-input-from-string(*standard-input* "\"string\"") ; string
    (read-as-string))
=> "\"string\""

#?(with-input-from-string(*standard-input* "\"nested \\\"double\\\" quotes\"") ; nested double quotes
    (read-as-string))
=> "\"nested \\\"double\\\" quotes\""

#?(with-input-from-string(*standard-input* "'foo") ; quoted symbol
    (read-as-string))
=> "'foo"

#?(with-input-from-string(*standard-input* "'(foo)") ; quoted list
    (read-as-string))
=> "'(foo)"

#?(with-input-from-string(*standard-input* "3.14") ; float
    (read-as-string))
=> "3.14"

#?(with-input-from-string(*standard-input* "#5r1234") ; ratio dispatch
    (read-as-string))
=> "#5r1234"

#?(with-input-from-string(*standard-input* "#O01234567") ; octal dispatch
    (read-as-string))
=> "#O01234567"

#?(with-input-from-string(*standard-input* "#P\"hoge/fuga.bar\"") ; pathname
    (read-as-string))
=> "#P\"hoge/fuga.bar\""

#?(with-input-from-string(*standard-input* "`(,foo ,(bar) ,@(bazz) ,.(hoge))") ; backquote
    (read-as-string))
=> "`(,foo ,(bar) ,@(bazz) ,.(hoge))"

#?(with-input-from-string(*standard-input* "#1A(1 2 3)") ; array dispatch
    (read-as-string))
=> "#1A(1 2 3)"

#?(with-input-from-string(*standard-input* "#S(structure :slot :value)") ; structure
    (read-as-string))
=> "#S(structure :slot :value)"

#?(with-input-from-string(*standard-input* "#+sbcl hoge") ; conditional +
    (read-as-string))
=> "#+sbcl hoge"

#?(with-input-from-string(*standard-input* "#-sbcl hoge") ; conditional -
    (read-as-string))
=> "#-sbcl hoge"

#?(with-input-from-string(*standard-input* "#+(or clisp ecl) hoge") ; compound conditional
    (read-as-string))
=> "#+(or clisp ecl) hoge"

#?(with-input-from-string(*standard-input* "; comment") ; line comment
    (read-as-string))
=> "; comment"

#?(with-input-from-string(*standard-input* (format nil "(; comment in s-expression~%)")) ; line comment is s-expression
    (read-as-string))
=> "(; comment in s-expression
)"

#?(with-input-from-string(*standard-input* "#|block comment|#") ; block comment
    (read-as-string))
=> "#|block comment|#"

#?(with-input-from-string(*standard-input* "#|nested#|block|#comment|#") ; nested block comment
    (read-as-string))
=> "#|nested#|block|#comment|#"

#?(with-input-from-string(*standard-input* "#|unbalanced#|nested comment|#") ; unbalanced nested comment
    (read-as-string))
:signals end-of-file

#?(with-input-from-string(*standard-input* "(#|block comment in s-expression|#)") ; block comment is s-expression
    (read-as-string))
=> "(#|block comment in s-expression|#)"

#?(with-input-from-string(*standard-input* ":foo") ; keyword
    (read-as-string))
=> ":foo"

#?(with-input-from-string(*standard-input* "#C(1 2)") ; complex
    (read-as-string))
=> "#C(1 2)"

#?(with-input-from-string(*standard-input* "#B1111") ; binary
    (read-as-string))
=> "#B1111"

#?(with-input-from-string(*standard-input* "#<unreadable object>") ; unreadable object
    (read-as-string))
=> "#<unreadable object>"

; NOTE!
#?(with-input-from-string(*standard-input* "#<FUNCTION > >") ; known bug
    (read-as-string))
=> "#<FUNCTION >"

(requirements-about READ-STRING-TILL :test string=
		    :around (with-input-from-string(*standard-input* "foo bar bazz")
			      (call-body)))

;;;; Description:
; read characters from stream till pred is satisfied.
#?(read-string-till (lambda(c)(char= c #\space)))
=> "foo"

#+syntax
(READ-STRING-TILL pred &optional (*standard-input* *standard-input*) (eof-error-p t) (eof-value nil) (consume nil)(include T)) ; => result

;;;; Arguments and Values:

; pred := function designator, otherwise error
#?(read-string-till (complement #'alpha-char-p))
=> "foo"

#?(read-string-till "GRAPHIC-CHAR-P")
:signals error
,:ignore-signals warning

; *standard-input* := input stream, otherwise error.
#?(read-string-till #'digit-char-p "foo123")
:signals error
,:ignore-signals warning

; eof-error-p := boolean
; specify signaling error when reach end of file.
; The default is T
#?(read-string-till #'alpha-char-p)
=> ""

#?(read-string-till #'digit-char-p)
=> "foo bar bazz"

#?(with-input-from-string(s "")
    (read-string-till #'alpha-char-p s))
:signals end-of-file

#?(with-input-from-string(s "")
    (read-string-till #'alpha-char-p s nil))
=> NIL
,:test eq

; eof-value := T, return value when reach end of file.
; The default is NIL.
#?(with-input-from-string(s "")
    (read-string-till #'digit-char-p s nil :return-value))
=> :return-value
,:test eq

; consume := boolean, specify character which satisfies pred is consumed or not.
; The default is NIL.
#?(progn (read-string-till (complement #'alpha-char-p))
	 (read-char))
=> #\space
,:test char=

#?(progn (read-string-till (complement #'alpha-char-p) *standard-input* t t t)
	 (read-char))
=> #\b
,:test char=

; include := boolean, specify character which satisifies pred is included to result or not.
; The default is T.
#?(read-string-till (complement #'alpha-char-p))
=> "foo"
#?(read-string-till (complement #'alpha-char-p)*standard-input* t t t)
=> "foo "
#?(values (read-string-till (complement #'alpha-char-p)*standard-input* t t t nil)
	  (read-char))
:values ("foo" #\b)
,:test equal

; result := string when success.
; signaling end-of-file when fail and eof-error-p is T (the default).
; T when fail and eof-error-p is NIL.

;;;; Affected By:
; none

;;;; Side-Effects:
; consume input stream contents

;;;; Notes:
; When character is escaped, such characters are always consumed.
#?(with-input-from-string(s "\\123")
    (read-string-till #'digit-char-p s))
=> "\\1"

; Do not confuse "\" and "\\", especially when it is string literal.
#?(with-input-from-string(s "[foo \] bar]")
    (read-string-till (lambda(c)(char= #\] c)) s t t t))
=> "[foo ]"
#?(with-input-from-string(s "[foo \\] bar]")
    (read-string-till (lambda(c)(char= #\] c)) s t t t))
=> "[foo \\] bar]"

;;;; Exceptional-Situations:

(requirements-about SPACE-CHAR-P)

;;;; Description:
; tests char is space character.
#?(every #'space-char-p '(#\space #\newline #\tab #\page #\return #\linefeed))
=> T

#+syntax
(SPACE-CHAR-P char) ; => result

;;;; Arguments and Values:

; char := character, otherwise error
#?(space-char-p " ") :signals error

; result := generarized boolean.
#?(space-char-p #\space) => #\space
#?(space-char-p #\b) => NIL

;;;; Affected By:
; read-as-string::*spaces*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TERMINAL-CHAR-P)

;;;; Description:
; Tests char is terminal character
#?(every #'terminal-char-p read-as-string::*terminals*)
=> T

#+syntax
(TERMINAL-CHAR-P char) ; => result

;;;; Arguments and Values:

; char := character, otherwise error.
#?(terminal-char-p " ") :signals error

; result := generarized boolean
#?(terminal-char-p #\space) => #\space
#?(terminal-char-p #\b) => NIL

;;;; Affected By:
; read-as-string::*terminals*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about COMMENTP)

;;;; Description:
; accepts string, tests such string is comment or not.
#?(commentp " ; line comment") => T
#?(commentp " #|block comment|#") => T
#?(commentp "#+comment hoge") => NIL

#+syntax
(COMMENTP string) ; => result

;;;; Arguments and Values:

; string := string otherwise error
#?(commentp :not-string) :signals error

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

