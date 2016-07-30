(defpackage :read-as-string.spec(:use :cl :read-as-string :jingoh))
(in-package :read-as-string.spec)
(setup :read-as-string)

(requirements-about read-as-string)

#|
read one S-Expression from stream like CL:READ.
But does not construct any lisp object but string.
|#
#?(with-input-from-string(s "foo")
    (read-as-string s))
=> "foo"
,:test string=

#?(with-input-from-string(s "(1 2 3 4)")
    (read-as-string s))
=> "(1 2 3 4)"
, :test string=

#|
When file toplevel comment comes, read it as S-Expression.
|#
#?(with-input-from-string(s "; line comment")
    (read-as-string s))
=> "; line comment"
, :test string=

#?(with-input-from-string(s (format nil "#|block~%comment~%|#"))
    (read-as-string s))
=> "#|block
comment
|#"
, :test string=

#|
Comment which is contained by S-Expression remains.
|#
#?(with-input-from-string(s "(foo #|comment|# bar)")
    (read-as-string s))
=> "(foo #|comment|# bar)"
, :test string=

#|
Unknown macro character remains.
|#
#?(with-input-from-string(s "#@hogehoge")
    (read-as-string s))
=> "#@hogehoge"
, :test string=

#|
Same with CL:READ, when reaches end of file, an error of type end-of-file is signaled.
|#
#?(with-input-from-string(s "(foo")
    (read-as-string s))
:signals end-of-file

#|
Same with CL:READ, when optional second parameter specified nil, error is not signaled.
|#
#?(with-input-from-string(s "")
    (read-as-string s nil))
=> NIL

#|
Same with CL:READ, when optional second parameter specified nil, and optional third parameter specified with arbitrary lisp object, such object is returned if reach end-of-file.
|#
#?(with-input-from-string(s "")
    (read-as-string s nil :end))
=> :end

#|
conditional reader (i.e. #- #+) remains.
|#
#?(with-input-from-string(s "#+(or clisp ccl sbcl ecl):foo")
    (read-as-string s))
=> "#+(or clisp ccl sbcl ecl):foo"
, :test string=
