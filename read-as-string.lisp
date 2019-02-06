(in-package :cl-user)
(defpackage :read-as-string(:use :cl :core-reader)
  (:import-from :fields #:Propagatef)
  (:export
    ; main api.
    #:read-as-string
    ; internal useful helpers.
    #:commentp
	   ))
(in-package :read-as-string)

(pushnew :read-as-string *features*)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defvar *spaces* '(#\space #\newline #\tab #\page #\return #\linefeed)))
(defvar *terminal-macro-chars* '(#\" #\' #\( #\) #\, #\; #\`))
(defvar *terminals* (append *spaces* *terminal-macro-chars*))

(declaim (ftype (function (&optional stream boolean t boolean)
			  (or string t))
		read-as-string))

(defun read-as-string(&optional(*standard-input* *standard-input*)
		       (eof-error-p T)
		       (eof-value nil)
		       (recursive-p nil))
  (handler-case(funcall(parser(list (peek-char(not recursive-p)))))
    (end-of-file(condition)(if eof-error-p
			     (error condition)
			     eof-value))))

(defvar *parsers*(make-hash-table :test #'equal))

(defun parser(cons &optional(default #'default-parser))
  (gethash cons *parsers* default))

(defun default-parser()
  (Read-string-till (Delimiter *terminals*)))

(eval-when(:load-toplevel :compile-toplevel :execute)
  (defun upcase(cons)
    (cons(car cons)(if(cdr cons)
		     (char-upcase(cdr cons))
		     (cdr cons))))

  (defmacro defparser(cons &body body)
    `(PROGN
       (SETF(GETHASH ',cons *PARSERS*)
	 (LAMBDA () ,@body))
       (COPY-PARSER ',(upcase cons) ',cons))))

(defun copy-parser(dest src)
  (Propagatef(gethash src *parsers*)
    (gethash dest *parsers*)
    (gethash (upcase dest) *parsers*)))

(defparser(#\space)
  (concatenate 'string
	       (Read-string-till(complement (Delimiter *spaces*)))
	       (read-as-string *standard-input* T T T)))

(progn . #.(map 'list(lambda(c)
		       `(COPY-PARSER '(,c)'(#\space)))
		*spaces*))

(defparser(#\()
  (loop :with open = 0
	:with close = 0
	:for c = (peek-char)
	:if(char= #\( c)
	:do(incf open):and :collect (read-char) :into result
	:else :if(char= #\) c)
	:do(incf close) :and :collect (read-char) :into result
	:else :collect (read-as-string *standard-input* T T T)
	:into result
	:until(= open close)
	:finally (return(String-concat result))))

(defparser(#\`)
  (concatenate 'string(string(read-char))
	       (read-as-string *standard-input* T T T)))

(progn . #.(mapcar(lambda(c)
		    `(COPY-PARSER '(,c) '(#\`)))
	     '(#\' #\,)))

(defparser(#\")
  (Read-delimited-string (read-char)))

(progn . #.(mapcar (lambda(c)
		     `(COPY-PARSER '(,c)'(#\")))
		   '(#\|)))

(defparser(#\;)
  (Read-string-till (Char-pred #\newline) *standard-input* T T T T))

(defparser(#\#)
  (concatenate 'string "#"
	       (funcall(parser (cons (read-char) ; consume #\#.
				     (peek-char))
			       (lambda() ; as default
				 (concatenate 'string (string(read-char))
					      (read-as-string *standard-input* T T T)))))))


(defparser(#\# . #\\)
  (uiop:strcat (read-char)
	       (read-char)
	       (unless(funcall(Delimiter *terminals*)(peek-char))
		 (Read-string-till (Delimiter *terminals*)))))

(defparser(#\# . #\()
  (read-as-string *standard-input* T T T))

(defparser(#\# . #\<)
  (Read-string-till (Char-pred #\>) *standard-input* T T T T))

(defparser(#\# . #\|)
  (with-output-to-string(*standard-output*)
    (write-char(read-char)) ; consume #\|
    (loop (write-string(read-string-till (lambda(c)(find c '(#\| #\#)))))
	  (let((c(read-char)) ; consume delimiter(i.e. #\| or #\#).
	       (next(read-char)))
	    (cond
	      ((and (char= #\| c)
		    (char= #\# next)) ; terminal.
	       (write-char c)(write-char next)(return nil))
	      ((and (char= #\# c)
		    (char= #\| next)) ; nested.
	       (write-char c)(unread-char next)
	       (write-string (funcall(parser(cons c next))))) ; recurse.
	      (t(write-char c)
		(if(find next '(#\| #\#))
		  (unread-char next)
		  (write-char next))))))))

(defparser(#\# . #\0)
  (multiple-value-call #'uiop:strcat
    (read-string-till(complement #'digit-char-p))
    (let((c(read-char)))
      (case c
	((#\a #\A #\* #\= #\r #\R)
	 (values c (read-as-string)))
	((#\#) ; labelling.
	 c)
	(otherwise ; user defined dispatch macro character comes.
	  (values c (read-as-string)))))))

(progn . #.(map 'list(lambda(c)
		       `(COPY-PARSER '(#\# . ,c) '(#\# . #\0)))
		"123456789"))

(defparser(#\# . #\+)
  (apply #'concatenate 'string (string(read-char))
	 (loop :with count = 0
	       :for sexp = (read-as-string *standard-input* T T T)
	       :collect sexp :into result
	       :unless (commentp sexp)
	       :do (incf count)
	       :when (= 2 count)
	       :do (loop-finish)
	       :finally (return result))))

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

(copy-parser '(#\# . #\-) '(#\# . #\+))
