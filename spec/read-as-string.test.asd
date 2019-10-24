; vim: ft=lisp et
(in-package :asdf)
(defsystem "read-as-string.test"
  :version
  "0.5.0"
  :depends-on
  (:jingoh "read-as-string")
  :components
  ((:file "read-as-string"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :read-as-string args)))
