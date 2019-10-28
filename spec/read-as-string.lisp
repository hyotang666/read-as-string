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
(READ-AS-STRING &optional stream (eof-error-p t) (eof-value nil) (recursive-p nil)) ; => result

;;;; Arguments and Values:

; stream := (or null stream)
#?(read-as-string "not (or null stream)") :signals condition

; When specified NIL, it treated as *standard-input*.
#?(with-input-from-string(*standard-input* ":example")
    (read-as-string nil))
=> ":example"

; The default is NIL.
#?(with-input-from-string(*standard-input* ":example")
    (read-as-string))
=> ":example"

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

#?(with-input-from-string(s "  () ")
    (read-as-string s t t t))
=> unspecified ; "  ()" or error.

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

#?(with-input-from-string(*standard-input* "|a')|") ; valid symbol.
    (read-as-string))
=> "|a')|"

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

#?(with-input-from-string(*standard-input* "#+unknown (list)")
    (read-as-string))
=> "#+unknown (list)"

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

#?(with-input-from-string(*standard-input* "#C(1 2)") ; complex
    (read-as-string))
=> "#C(1 2)"

#?(with-input-from-string(*standard-input* "#B1111") ; binary
    (read-as-string))
=> "#B1111"

#?(with-input-from-string(*standard-input* "#<unreadable object>") ; unreadable object
    (read-as-string))
:signals read-unreadable-object

; NOTE!
; When specify `*MUFFLE-READER-ERROR*` with T,
; READ-UNREADABLE-OBJECT is not signaled.
#?(with-input-from-string(*standard-input* "#<unreadable object>")
    (let((*muffle-reader-error*
	   T))
      (read-as-string)))
=> "#<unreadable object>"

; NOTE!
; In such case, returned string may not correct.
#?(with-input-from-string(*standard-input* "#<FUNCTION > >") ; known bug 1
    (let((*muffle-reader-error*
	   T))
      (values (read-as-string)
	      (read-as-string))))
:values ("#<FUNCTION >" ">")
,:test equal

#?(with-input-from-string(*standard-input* "#<FUNCTION \\> >")
    (let((*muffle-reader-error*
	   T))
      (read-as-string)))
=> "#<FUNCTION \\> >"

#?(with-input-from-string(*standard-input* "#Unknown Dispatcher") ; Unknown dispatch macro char
    (read-as-string))
:signals no-dispatch-function

; NOTE!
; When specify `*MUFFLE-READER-ERROR*` with T,
; NO-DISPATCH-FUNCTION  is not signaled.
#?(with-input-from-string(*standard-input* "#Unknown Dispatcher")
    (let((*muffle-reader-error*
	   T))
      (read-as-string)))
=> "#Unknown"

(requirements-about *MUFFLE-READER-ERROR* :doc-type variable)

;;;; Description:
; Variable to control reader error.
; When T, the reader error is muffled except end-of-file.
; This is useful to analyze lisp source which may include invalid form.

;;;; Value type is generalized boolean.

; Initial value is NIL

;;;; Affected By:
; READ-AS-STRING::|#reader|, READ-AS-STRING::|#<reader|.

;;;; Notes:

(requirements-about NO-DISPATCH-FUNCTION :doc-type TYPE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; no-dispatch-function reader-error parse-error stream-error cell-error error serious-condition condition slot-object t

;;;; Effective Slots:

; NAME [Type] T
; [READER] cell-error-name

; STREAM [Type] T
; [READER] stream-error-stream

;;;; Notes:

(requirements-about READ-UNREADABLE-OBJECT :doc-type TYPE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; read-unreadable-object reader-error parse-error stream-error error serious-condition condition slot-object t

;;;; Effective Slots:

; STREAM [Type] T
; [READER] stream-error-stream

;;;; Notes:

(requirements-about SET-DISPATCHER :doc-type function
		    :around (let((read-as-string::*dispatch-macros*
				   (make-hash-table)))
			      (call-body)))

;;;; Description:
; Same with CL:SET-DISPATCH-MACRO-CHARACTER.

#+syntax
(SET-DISPATCHER char fun) ; => result

;;;; Arguments and Values:

; char := character otherwise error.
#?(set-dispatcher "not-character" '#:dummy) :signals condition

; fun := (or function symbol) as (function(stream character (integer 0 *))string)
; otherwise error.
#?(set-dispatcher #\+ "not (or symbol function)") :signals condition

; result := (eql t)
#?(set-dispatcher #\+ 'list) => T

;;;; Affected By:

;;;; Side-Effects:
; Modify state of READ-AS-STRING::*DISPATCH-MACROS* internally.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about GET-DISPATCHER :doc-type function)

;;;; Description:
; Like CL:GET-DISPATCH-MACRO-CHARACTER.

#+syntax
(GET-DISPATCHER char) ; => result

;;;; Arguments and Values:

; char := character, otherwise condition.
#?(get-dispatcher "not character") :signals condition

; result := (or null (or symbol function))
#?(get-dispatcher #\') :be-the (or symbol function)
#?(get-dispatcher #\!) => NIL

;;;; Affected By:
; State of READ-AS-STRING::*DISPATCH-MACROS*.

;;;; Side-Effects:
; none

;;;; Notes:
; Now supported dispatcher is only #\#.

;;;; Exceptional-Situations:

(requirements-about READ-TOKEN :doc-type function)

;;;; Description:
; Read token from STREAM.
; About `token`, see http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm

#+syntax
(READ-TOKEN &optional (*standard-input* *standard-input*)) ; => result

#?(with-input-from-string(*standard-input* "token")
    (read-token))
=> "token"
,:test equal

#?(with-input-from-string(*standard-input* "token\\single-escaped")
    (read-token))
=> "token\\single-escaped"
,:test equal

#?(with-input-from-string(*standard-input* "|multiple escaped|")
    (read-token))
=> "|multiple escaped|"
,:test equal

#?(with-input-from-string(*standard-input* "token|multiple escaped|")
    (read-token))
=> "token|multiple escaped|"
,:test equal

#?(with-input-from-string(*standard-input* "|multiple escaped|token")
    (read-token))
=> "|multiple escaped|token"
,:test equal

#?(with-input-from-string(*standard-input* "token|multiple escaped|token")
    (read-token))
=> "token|multiple escaped|token"
,:test equal

#?(with-input-from-string(*standard-input* "|e.g. ' never treated as reader macro|")
    (read-token))
=> "|e.g. ' never treated as reader macro|"
,:test equal

#?(with-input-from-string(*standard-input* "|escaped|not-escaped|escaped|")
    (read-token))
=> "|escaped|not-escaped|escaped|"
,:test equal

#?(with-input-from-string(*standard-input* "012")
    (read-token))
=> "012"
,:test equal

#?(with-input-from-string(*standard-input* "012.")
    (read-token))
=> "012."
,:test equal

#?(with-input-from-string(*standard-input* "012.345")
    (read-token))
=> "012.345"
,:test equal

;;;; Arguments and Values:

; *standard-input* := input stream, otherwise error.
#?(read-token "not stream") :signals condition

; result := string

;;;; Affected By:

;;;; Side-Effects:
; May consume stream contents.

;;;; Notes:
; When whitespaces or terminal character comes first,
; empty string is returned.
#?(with-input-from-string(*standard-input* "")
    (read-token))
=> ""
,:test equal

; Whitespaces or terminal characters are never consumed.
#?(with-input-from-string(*standard-input* " ")
    (values (read-token)
	    (read-char)))
:values ("" #\space)

#?(with-input-from-string(*standard-input* "'<--- terminal character")
    (read-token))
=> ""
,:test equal

#?(with-input-from-string(*standard-input* "a#<---Non-terminal")
    (read-token))
=> "a#<---Non-terminal"
,:test equal

; Never check notation validity.
#?(with-input-from-string(*standard-input* "non-existant-package::symbol")
    (read-token))
=> "non-existant-package::symbol"
,:test equal

#?(with-input-from-string(*standard-input* "invalid:::colon")
    (read-token))
=> "invalid:::colon"
,:test equal

;;;; Exceptional-Situations:
