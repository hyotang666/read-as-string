; vim: ft=lisp et
(in-package :asdf)

(defsystem :read-as-string
  :version "2.2.7"
  :author "Shinichi Sato"
  :description "Reading S-Expression string from stream."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on
  (
   "named-readtables" ; Readtable manager.
   "core-reader"        ; utilities for making stream reader.
   "uiop"               ; utilities.
   )
  :components((:file "read-as-string")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "read-as-string"))))
  (append (call-next-method) '((test-op "read-as-string.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "read-as-string")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
