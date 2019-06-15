; vim: ft=lisp et
(in-package :asdf)

(defsystem :read-as-string
  :version "0.0.1"
  :author "Shinichi Sato"
  :description "Reading S-Expression string from stream."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on
  (
   "core-reader"        ; utilities for making stream reader.
   "uiop"               ; utilities.
   "fields"             ; field utilities.
   )
  :components((:file "read-as-string")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "read-as-string"))))
  (append (call-next-method)'((test-op "read-as-string.test"))))
(defmethod operate :around(o (c (eql (find-system "read-as-string")))
                             &key ((:compile-print *compile-print*))
                             ((:compile-verbose *compile-verbose*))
                             &allow-other-keys)
  (call-next-method))
