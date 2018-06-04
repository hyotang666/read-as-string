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

(defmethod component-depends-on ((o test-op) (c (eql (find-system "read-as-string"))))
  (append (call-next-method)'((test-op "read-as-string.test"))))
