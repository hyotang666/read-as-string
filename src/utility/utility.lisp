(defpackage :ras.utility(:use :cl)
  (:export
    #:doc
    #:prototype
    ))
(in-package :ras.utility)

(defun doc(system pathname)
  (uiop:read-file-string
    (uiop:subpathname(asdf:system-source-directory(asdf:find-system system))
      pathname)))

(defmacro prototype(name param-types return-type)
  "C-style prototype declaration."
  `(DECLAIM(FTYPE(FUNCTION ,param-types ,return-type),name)))


