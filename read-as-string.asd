; vim: ft=lisp et
(defsystem :read-as-string
  :depends-on (ras.bsearch)
  :pathname "src/read-as-string/"
  :components((:file "read-as-string")))
