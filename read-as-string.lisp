(in-package :cl-user)
(defpackage :read-as-string(:use :cl :core-reader)
  (:export
    ; main api.
    #:read-as-string
    ; internal useful helpers.
    #:commentp
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
		     (cell-error-name condition)))))

(eval-when(:compile-toplevel :load-toplevel :execute)
  ;; See http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm#charsyntaxtypesinstdsyntax
  (defvar *spaces* '(#\space #\newline #\tab #\page #\return #\linefeed)))

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
      (concatenate 'string
		   (if recursive-p
		     (Read-string-till (complement (Delimiter *spaces*)))
		     "")
		   (if(get-macro-character char)
		     (read *standard-input* eof-error-p eof-value recursive-p)
		     (read-token))))))

(defun read-token(&optional (*standard-input* *standard-input*))
  (uiop:reduce/strcat
    (loop :for char := (peek-char nil nil nil nil)
	  :while char
	  :if (or (find char *spaces*)
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
  (declare(ignore character))
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
      (error 'no-dispatch-function :name char))))

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

(defun add-dispatcher(char fun)
  (setf (gethash (char-upcase char)
		 *dispatch-macros*)
	fun))

(defun get-dispatcher(char)
  (values (gethash (char-upcase char)
		   *dispatch-macros*)))

(add-dispatcher #\# '|##reader|)
(add-dispatcher #\( '|#paren-reader|)
(add-dispatcher #\= '|#=reader|)
(add-dispatcher #\+ '|#+reader|)
(add-dispatcher #\- '|#+reader|)
(add-dispatcher #\* '|#=reader|)
(add-dispatcher #\\ '|#=reader|)
(add-dispatcher #\' '|#=reader|)
(add-dispatcher #\) (get-dispatch-macro-character #\# #\) (copy-readtable nil)))
(add-dispatcher #\< (or (get-dispatch-macro-character #\# #\< (copy-readtable nil))
			'|#<reader|))
(add-dispatcher #\: '|#=reader|)
(add-dispatcher #\| '|#\|reader|)
(add-dispatcher #\. '|#=reader|)
(add-dispatcher #\A '|#=reader|)
(add-dispatcher #\B '|#=reader|)
(add-dispatcher #\C '|#=reader|)
(add-dispatcher #\O '|#=reader|)
(add-dispatcher #\P '|#=reader|)
(add-dispatcher #\R '|#=reader|)
(add-dispatcher #\S '|#=reader|)
(add-dispatcher #\X '|#=reader|)

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
			       (Read-string-till #'digit-char-p)))
			  (acc (|#\|reader| stream
					    (read-char stream)
					    digit))))
		       (otherwise (acc char))))
		    (otherwise (acc char)))))))

(defun |#+reader|(stream character number)
  (format nil "#~@[~A~]~C~A~A"
	  number
	  character
	  (read-as-string stream t t t)
	  (read-as-string stream t t t)))

;;; For ECL.
(defun |#<reader|(stream character number)
  (declare(ignore stream character number))
  (error 'reader-error))

(defun commentp(string)
  #+ecl(check-type string string)
  (labels((LINE-COMMENTP(string)
	    (with-input-from-string(s string)
	      (char= #\; (peek-char t s))))
	  (BLOCK-COMMENTP(string)
	    (with-input-from-string(s string)
	      (and (char= #\# (peek-char t s))
		   (char= #\| (progn (read-char s) ; discard #\#
				     (read-char s)))))))
    (or (LINE-COMMENTP string)
	(BLOCK-COMMENTP string))))
