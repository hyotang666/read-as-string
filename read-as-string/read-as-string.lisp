(in-package :cl-user)
(defpackage :read-as-string(:use :cl)
  (:import-from :fields #:Propagatef)
  (:import-from :ras.bsearch #:Bsearch)
  (:import-from :type-ext #:Prototype)
  (:export
    ; main api.
    #:read-as-string
    ; internal useful helpers.
    #:read-string-till
    #:space-char-p
    #:terminal-char-p
    #:commentp
	   ))
(in-package :read-as-string)

;;;; develop utility
(define-symbol-macro u (asdf:load-system :read-as-string))

(pushnew :read-as-string *features*)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defvar *spaces* (sort #(#\space #\newline #\tab #\page #\return #\linefeed)
			 #'char<)))
(defvar *terminal-macro-chars* (sort #(#\" #\' #\( #\) #\, #\; #\`)
				     #'char<))
(defvar *terminals* (sort(concatenate 'vector *spaces* *terminal-macro-chars*)
		      #'char<))

(Prototype read-as-string(&optional stream boolean t boolean)(or string t))
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
  (read-string-till #'terminal-char-p))

(defun terminal-char-p(char)
  (Bsearch char *terminals* :test #'char= :compare #'char<))

(deftype function-designator()
  '(or function
       (and symbol (not (or boolean keyword)))))

(Prototype read-string-till(function-designator
			    &optional stream boolean T boolean boolean)
	   (values (or string t) boolean))
(defun read-string-till (pred &optional
			      (*standard-input* *standard-input*)
			      (eof-error-p t)
			      (eof-value nil)
			      (consume nil)
			      (include t))
  (loop :for (c condition) = (multiple-value-list(ignore-errors(read-char)))
	;; C is character or nil.
	:while(or (and c (null(funcall pred c)))
		  (and (null c) ; reach end of file.
		       (if result
			 (return(coerce result 'string))
			 (if eof-error-p
			   (error condition)
			   (return eof-value)))))
	:collect c :into result ; always consume one char.
	:when(char= c #\\) ; escape char-p
	:collect (read-char) :into result ; consume one more char.
	:finally(return (if consume
			  (if include
			    (concatenate 'string result (string c))
			    (coerce result 'string))
			  (progn(unread-char c)
			    (coerce result 'string))))))

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
	       (read-string-till(complement #'space-char-p))
	       (read-as-string *standard-input* T T T)))

(defun space-char-p(char)
  (Bsearch char *spaces* :test #'char= :compare #'char<))

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
	:finally (return(uiop:reduce/strcat result))))

(defparser(#\`)
  (concatenate 'string(string(read-char))
	       (read-as-string *standard-input* T T T)))

(progn . #.(mapcar(lambda(c)
		    `(COPY-PARSER '(,c) '(#\`)))
	     '(#\' #\,)))

(defparser(#\")
  (concatenate 'string(string(read-char))
	       (read-string-till (lambda(c)(char= #\" c))
				 *standard-input* T T T)))

(defparser(#\|)
  (concatenate 'string(string(read-char))
	       (read-string-till (lambda(c)(char= #\| c))
				 *standard-input* T T T)))

(defparser(#\;)
  (read-string-till (lambda(c)(char= #\newline c))
		    *standard-input* T T T))

(defparser(#\#)
  (concatenate 'string "#"
	       (funcall(parser (cons (read-char) ; consume #\#.
				     (peek-char))
			       (lambda() ; as default
				 (concatenate 'string (string(read-char))
					      (read-as-string)))))))

(defparser(#\# . #\')
  (concatenate 'string (string(read-char))
	       (read-as-string *standard-input* T T T)))

(progn . #.(mapcar(lambda(c)
		    `(COPY-PARSER '(#\# . ,c) '(#\# . #\')))
	     '(#\b #\o #\x #\* #\:)))

(defparser(#\# . #\\)
  (uiop:strcat (read-char)
	       (read-char)
	       (unless(terminal-char-p(peek-char))
		 (read-string-till #'terminal-char-p))))

(defparser(#\# . #\()
  (read-as-string *standard-input* T T T))

(defparser(#\# . #\.)
  (concatenate 'string(string(read-char))
	       (read-as-string *standard-input* T T T)))

(progn . #.(mapcar(lambda(c)
		    `(COPY-PARSER '(#\# . ,c) '(#\# . #\.)))
	     '(#\s #\p #\a #\c #\:)))

(defparser(#\# . #\<)
  (read-string-till (lambda(c)(char= #\> c))
		    *standard-input* T T T))

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
