(in-package :cl-user)
(defpackage :read-as-string(:use :cl :core-reader)
  (:export
    ;; main api.
    #:read-as-string

    ;; variables.
    #:*muffle-reader-error*

    ;; conditions
    #:no-dispatch-function
    #:read-unreadable-object

    ;; readtable name
    #:as-string

    ;; dispatcher
    #:set-dispatcher
    #:get-dispatcher

    ;; internal useful helpers.
    #:read-token
    ))
(in-package :read-as-string)

(pushnew :read-as-string *features*)

;;;; VARIABLES
(defvar *muffle-reader-error* nil)

;;;; CONDITIONS
(define-condition no-dispatch-function(reader-error cell-error)
  ()
  (:report (lambda(condition stream)
	     (format stream "No dispatch function defined for ~S"
		     (cell-error-name condition))
	     (let((in
		    (stream-error-stream condition)))
	       (if(typep in 'file-stream)
		 (format stream "~%File: ~S" (pathname in))
		 (format stream "~%Stream: ~S" in))))))

(define-condition read-unreadable-object(reader-error)
  ()
  (:report (lambda(condition stream)
	     (let((s
		    (stream-error-stream condition)))
	       (format stream "Unreadable object comes. \"#<\"~%")
	       (if(typep s 'file-stream)
		 (format stream "File: ~S" (pathname s))
		 (format stream "Stream: ~S" s))))))

(declaim(ftype (function (character)
			 (values (or null character)
				 &optional))
	       whitecharp))
(setf (symbol-function 'whitecharp)
      (Delimiter '(#\space #\newline #\tab #\page #\return #\linefeed)))

;;;; READ-AS-STRING
(declaim (ftype (function (&optional stream boolean t boolean)
			  (or string t))
		read-as-string))
(defun read-as-string(&optional(*standard-input* *standard-input*)
		       (eof-error-p T)
		       (eof-value nil)
		       (recursive-p nil))
  (let((*readtable*
	 (named-readtables:find-readtable 'as-string))
       (char
	 (peek-char (null recursive-p)
		    *standard-input*
		    eof-error-p
		    eof-value
		    recursive-p)))
    (if(eq char eof-value)
      char
      (multiple-value-call #'concatenate
	'string
	(if recursive-p
	  (Read-string-till (complement #'whitecharp))
	  "")
	(if(get-macro-character char)
	  (read *standard-input* eof-error-p eof-value recursive-p)
	  (read-token))))))

(declaim(ftype (function (&optional stream)
			 (values string &optional))
	       read-token))
(defun read-token(&optional (*standard-input* *standard-input*))
  (String-concat
    (loop :for char := (peek-char nil nil nil nil)
	  :while char
	  :if (or (whitecharp char)
		  (multiple-value-bind(macro non-terminal-p)(get-macro-character char)
		    (and macro
			 (not non-terminal-p))))
	  :do (loop-finish)
	  :else :if (char= #\\ char)
	  :collect (read-char) :and :collect (read-char)
	  :else :if (char= #\| char)
	  :collect (Read-delimited-string (read-char))
	  :else :collect (read-char))))

;;; MACRO CHARS
(defun |"reader|(stream character)
  (prin1-to-string(funcall (load-time-value
			     (get-macro-character #\"(copy-readtable nil))
			     T)
			   stream
			   character)))

(defun |'reader|(stream character)
  (format nil "~C~A"
	  character
	  (read-as-string stream)))

(defun |paren-reader|(stream character)
  (declare(ignore character))
  (let((*print-pretty*)) ; For CLISP.
    (format nil "(~{~A~}"
	    (loop :for char = (peek-char nil stream)
		  :if (char= #\) char)
		  :collect (read-char stream)
		  :and :do (loop-finish)
		  :else :if (char= #\. char)
		  :collect (read-char stream)
		  :else :collect (read-as-string stream t t t)))))

(defun |`reader|(stream character)
  (format nil "~C~A"
	  character
	  (read-as-string stream)))

(defun |;reader|(stream character)
  (format nil "~C~A"
	  character
	  (Read-string-till (Char-pred #\newline) stream T T T T)))

(defun |#reader|(stream character)
  (let*((digit
	  (Read-string-till (complement #'digit-char-p)
			    stream))
	(char
	  (peek-char nil stream))
	(reader
	  (get-dispatcher char)))
    (if reader
      (funcall reader
	       stream
	       (read-char stream)
	       digit)
      (if *muffle-reader-error*
	(format nil "~C~A"
		character
		(read-as-string stream t t t))
	(error 'no-dispatch-function :name char :stream stream)))))

;;;; READTABLE
(named-readtables:defreadtable as-string
  (:macro-char #\" '|"reader|)
  (:macro-char #\' '|'reader|)
  (:macro-char #\( '|paren-reader|)
  (:macro-char #\` '|`reader|)
  (:macro-char #\; '|;reader|)
  (:macro-char #\# '|#reader|)
  (:syntax-from :common-lisp #\) #\))
  )

(defvar *dispatch-macros* (make-hash-table))

(declaim(ftype (function (character (or symbol function))
			 (values (eql t) &optional))
	       set-dispatcher))
(defun set-dispatcher(char fun)
  #+clisp
  (check-type fun (or symbol function))
  (setf (gethash (char-upcase char)
		 *dispatch-macros*)
	fun)
  T)

(defun get-dispatcher(char)
  (values (gethash (char-upcase char)
		   *dispatch-macros*)))

(set-dispatcher #\# '|##reader|)
(set-dispatcher #\( '|#paren-reader|)
(set-dispatcher #\= '|#=reader|)
(set-dispatcher #\+ '|#+reader|)
(set-dispatcher #\- '|#+reader|)
(set-dispatcher #\* '|#=reader|)
(set-dispatcher #\\ '|#=reader|)
(set-dispatcher #\' '|#=reader|)
(set-dispatcher #\) (get-dispatch-macro-character #\# #\) (copy-readtable nil)))
(set-dispatcher #\< '|#<reader|)
(set-dispatcher #\: '|#=reader|)
(set-dispatcher #\| '|#\|reader|)
(set-dispatcher #\. '|#=reader|)
(set-dispatcher #\A '|#=reader|)
(set-dispatcher #\B '|#=reader|)
(set-dispatcher #\C '|#=reader|)
(set-dispatcher #\O '|#=reader|)
(set-dispatcher #\P '|#=reader|)
(set-dispatcher #\R '|#=reader|)
(set-dispatcher #\S '|#=reader|)
(set-dispatcher #\X '|#=reader|)

;;; DISPATCH MACRO CHARS
(defun |##reader|(stream character number)
  (declare(ignore stream))
  (format nil "#~@[~A~]~C"
	  number
	  character))

(defun |#paren-reader|(stream character number)
  (format nil "#~@[~A~]~A"
	  number
	  (|paren-reader| stream character)))

(defun |#=reader|(stream character number)
  (format nil "#~@[~A~]~C~A"
	  number
	  character
	  (read-as-string stream t t t)))

(defun |#\|reader|(stream character number)
  (format nil "#~@[~A~]~C~{~A~}"
	  number
	  character
	  (uiop:while-collecting(acc)
	    (loop :for char = (read-char stream)
		  :do
		  (case char
		    (#\|
		     (acc char)
		     (when(char= #\# (peek-char nil stream))
		       (acc (read-char stream))
		       (loop-finish)))
		    (#\#
		     (case (peek-char nil stream)
		       (#\| ; nested comment
			(acc (|#\|reader| stream (read-char stream)
					  nil)))
		       ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
			(let((digit
			       (Read-string-till (complement #'digit-char-p)))
			     (char
			       (peek-char nil stream)))
			  (if(char= #\| char) ; nested comment with digit.
			    (acc (|#\|reader| stream
					      (read-char stream)
					      digit))
			    (acc digit))))
		       (otherwise (acc char))))
		    (otherwise (acc char)))))))

(defun |#+reader|(stream character number)
  (format nil "#~@[~A~]~C~A~A"
	  number
	  character
	  (read-as-string stream t t t)
	  (read-as-string stream t t t)))

(defun |#<reader|(stream character number)
  (if *muffle-reader-error*
    (format nil "#~A~C~A"
	    number
	    character
	    (Read-string-till (Char-pred #\>)
			      stream
			      t
			      t
			      t
			      t))
    (error 'read-unreadable-object :stream stream)))
