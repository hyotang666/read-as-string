; vim: ft=lisp et
(in-package :asdf)
(defsystem :read-as-string.test
  :version "0.0.0"
  :depends-on (:jingoh "read-as-string")
 :components ((:file "read-as-string")) :perform
 (test-op (o c) (symbol-call :jingoh :examine :read-as-string)))
