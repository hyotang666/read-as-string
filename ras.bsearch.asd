; vim: ft=lisp et
(defsystem :ras.bsearch
  :pathname "src/bsearch/"
  :depends-on (:ras.utility)
  :components((:file "bsearch")))
