; vim: ft=lisp et
(eval-when(:compile-toplevel :load-toplevel :execute)
  (let*((default(or *compile-file-pathname* *load-pathname*))
        (pathname(uiop:subpathname default "README.md")))
    (defun description()
      (when(probe-file pathname)
        (let((line(uiop:read-file-line pathname)))
          (subseq line (+ 2 (search "- " line))))))

    (defun long-description()
      (when(probe-file pathname)
        (uiop:read-file-string pathname)))))

(defsystem :read-as-string
  :description #.(description)
  :long-description #.(long-description)
  :in-order-to ((test-op (test-op :read-as-string.test)))
  :depends-on (ras.utility ras.bsearch)
  :pathname "../src/read-as-string/"
  :components((:file "read-as-string")))
