; vim: ft=lisp et
(in-package :asdf)

(defsystem :read-as-string
  :depends-on (:ras.bsearch :uiop :fields)
  :components((:file "read-as-string")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "read-as-string"))))
 (test-system :read-as-string.test))