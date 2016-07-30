; vim: ft=lisp et
(in-package :asdf)
(defsystem :read-as-string.test
  :pathname "../src/read-as-string/"
  :depends-on (:jingoh)
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'report)))
