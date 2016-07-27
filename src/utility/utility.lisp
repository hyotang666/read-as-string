(defpackage :ras.utility(:use :cl)
  (:export
    #:doc
    #:prototype
    #:propagatef
    ))
(in-package :ras.utility)

(defun doc(system pathname)
  (uiop:read-file-string
    (uiop:subpathname(asdf:system-source-directory(asdf:find-system system))
      pathname)))

(defmacro prototype(name param-types return-type)
  "C-style prototype declaration."
  `(DECLAIM(FTYPE(FUNCTION ,param-types ,return-type),name)))

(defmacro propagatef(item &rest places)
  "Set ITEM to all PLACES."
  (let((var(gensym"VAR")))
    `(LET((,var ,item))
       (SETF ,@(mapcan(lambda(x)
			`(,x ,var))
		 places)))))

