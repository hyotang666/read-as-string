(in-package :cl-user)
(defpackage :read-as-string(:use :cl :ras.utility :ras.bsearch)
  (:export
    ; main api.
    #:read-as-string
    ; internal useful helpers.
    #:read-string-till
    #:space-char-p
    #:terminal-char-p
	   ))
(in-package :read-as-string)

(pushnew :read-as-string *features*)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defvar *spaces* (sort #(#\space #\newline #\tab #\page #\return #\linefeed)
			 #'char<)))
(defvar *terminal-macro-chars* (sort #(#\" #\' #\( #\) #\, #\; #\`)
				     #'char<))
(defvar *terminals* (sort(concatenate 'vector *spaces* *terminal-macro-chars*)
		      #'char<))

(prototype read-as-string(&optional stream boolean t boolean)(or string t))
(defun read-as-string(&optional(*standard-input* *standard-input*)
		       (eof-error-p T)
		       (eof-value nil)
		       (recursive-p nil))
  #.(doc :read-as-string "doc/read-as-string.md")
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
  #.(doc :read-as-string "doc/terminal-char-p.md")
  (bsearch char *terminals* :test #'char= :compare #'char<))

(prototype read-string-till(function &optional stream boolean T boolean)
	   (values (or string t) boolean))
(defun read-string-till (pred &optional (*standard-input* *standard-input*)
				  (eof-error-p t)
				  (eof-value nil)
				  (consume nil))
  #.(doc :read-as-string "doc/read-string-till.md")
  (loop :for (c . condition) = (multiple-value-list(ignore-errors(read-char)))
	;; C is character or nil.
	:while(or (and c (not(funcall pred c)))
		  (and (null c) ; reach end of file.
		       (if result
			 (return(coerce result 'string))
			 (if eof-error-p
			   (error (car condition))
			   (return eof-value)))))
	:collect c :into result ; always consume one char.
	:when(char= c #\\) ; escape char-p
	:collect (read-char) :into result ; consume one more char.
	:finally(return (if consume
			  (concatenate 'string result (string c))
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

(eval-when(:load-toplevel :compile-toplevel :execute)
  (defmacro propagatef(item &rest places)
    "Set ITEM to all PLACES."
    (let((var(gensym"VAR")))
      `(LET((,var ,item))
	 (SETF ,@(mapcan(lambda(x)
			  `(,x ,var))
		   places))))))

(defun copy-parser(dest src)
  (propagatef(gethash src *parsers*)
    (gethash dest *parsers*)
    (gethash (upcase dest) *parsers*)))

(defparser(#\space)
  (concatenate 'string
	       (read-string-till(complement #'space-char-p))
	       (read-as-string *standard-input* T T T)))

(defun space-char-p(char)
  #.(doc :read-as-string "doc/space-char-p.md")
  (bsearch char *spaces* :test #'char= :compare #'char<))

(progn . #.(map 'list(lambda(c)
		       `(COPY-PARSER '(,c)'(#\space)))
		*spaces*))

(defparser(#\()
  (loop :with open = 0
	:with close = 0
	:for c = (peek-char)
	:if(char= #\( c)
	:do(incf open):and :collect (string (read-char)) :into result
	:else :if(char= #\) c)
	:do(incf close) :and :collect (string(read-char)) :into result
	:else :collect (read-as-string *standard-input* T T T)
	:into result
	:until(= open close)
	:finally (return(apply #'concatenate 'string result))))

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
			       (lambda()
				 (concatenate 'string (string(read-char))
					      (read-as-string)))))))

(defparser(#\# . #\')
  (concatenate 'string (string(read-char))
	       (default-parser)))

(progn . #.(mapcar(lambda(c)
		    `(COPY-PARSER '(#\# . ,c) '(#\# . #\')))
	     '(#\b #\o #\x #\* #\:)))

(defparser(#\# . #\\)
  (concatenate 'string(string(read-char))
	       (string(read-char))
	       (if(terminal-char-p(peek-char))
		 ""
		 (read-string-till #'terminal-char-p))))

(defparser(#\# . #\()
  (read-as-string *standard-input* T T T))

(defparser(#\# . #\.)
  (concatenate 'string(string(read-char))
	       (read-as-string *standard-input* T T T)))

(progn . #.(mapcar(lambda(c)
		    `(COPY-PARSER '(#\# . ,c) '(#\# . #\.)))
	     '(#\s #\p #\a #\c)))

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
  (concatenate 'string
	       (read-string-till(complement #'digit-char-p))
	       (let((c(read-char)))
		 (case c
		   ((#\a #\A #\* #\= #\r #\R)
		    (concatenate 'string (string c) (read-as-string)))
		   ((#\#) ; labelling.
		    (string c))
		   (otherwise ; user defined dispatch macro character comes.
		     (concatenate 'string(string c)
				  (read-as-string)))))))

(progn . #.(map 'list(lambda(c)
		       `(COPY-PARSER '(#\# . ,c) '(#\# . #\0)))
		"123456789"))

(defparser(#\# . #\+)
  (apply #'concatenate 'string (string(read-char))
	 (loop :repeat 2
	       :collect (read-as-string *standard-input* T T T))))

(copy-parser '(#\# . #\-) '(#\# . #\+))
