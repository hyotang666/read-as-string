; vim: ft=lisp et
(in-package :asdf)

(defsystem :read-as-string
  :author "Shinichi Sato"
  :description "Reading S-Expression string from stream."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on ("bsearch" "core-reader" "type-ext" :uiop :fields)
  :components((:file "read-as-string")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "read-as-string"))))
 (test-system :read-as-string.test))
