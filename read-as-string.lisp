(in-package :cl-user)

(defpackage :read-as-string
  (:use :cl :core-reader)
  (:export ;; main api.
           #:read-as-string
           ;; variables.
           #:*muffle-reader-error*
           #:*default-readtable*
           ;; conditions
           #:no-dispatch-function
           #:read-unreadable-object
           ;; readtable name
           #:as-string
           ;; dispatcher
           #:set-dispatcher
           #:get-dispatcher
           ;; internal useful helpers.
           #:read-token))

(in-package :read-as-string)

(pushnew :read-as-string *features*)

(declaim (optimize speed))

;;;; VARIABLES

(defvar *muffle-reader-error* nil)

(defvar *default-readtable* 'as-string)

;;;; CONDITIONS

(define-condition no-dispatch-function (reader-error cell-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "No dispatch function defined for ~S"
             (cell-error-name condition))
     (let ((in (stream-error-stream condition)))
       (if (typep in 'file-stream)
           (format stream "~%File: ~S" (pathname in))
           (format stream "~%Stream: ~S" in))))))

(define-condition read-unreadable-object (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (let ((s (stream-error-stream condition)))
       (format stream "Unreadable object comes. \"#<\"~%")
       (if (typep s 'file-stream)
           (format stream "File: ~S" (pathname s))
           (format stream "Stream: ~S" s))))))

(setf (symbol-function 'whitecharp)
        (delimiter '(#\Space #\Newline #\Tab #\Page #\Return #\Newline)))

;;;; READ-AS-STRING

(declaim
 (ftype (function (&optional (or stream null) boolean t boolean)
         (values (or string t) &optional))
        read-as-string))

(defun read-as-string
       (&optional stream (eof-error-p t) (eof-value nil) (recursive-p nil))
  (let* ((*readtable* (named-readtables:find-readtable *default-readtable*))
         (*standard-input* (or stream *standard-input*)))
    (handler-case
        (with-output-to-string (*standard-output*)
          (%read-as-string *standard-input* eof-error-p eof-value recursive-p))
      (end-of-file (c)
        (if eof-error-p
            (error c)
            eof-value)))))

(defun %read-as-string
       (&optional stream (eof-error-p t) (eof-value nil) (recursive-p nil))
  (let* ((*standard-input* (or stream *standard-input*))
         (char (peek-char (null recursive-p) nil t t recursive-p)))
    (when recursive-p
      (do-stream-till (c (lambda (c) (not (whitecharp c))))
        (write-char c)
        (when (char= #\\ c)
          (write-char (read-char)))))
    (if (named-readtables::%get-macro-character
          (if recursive-p
              (peek-char (null recursive-p) nil t t recursive-p)
              char)
          *readtable*)
        (read *standard-input* eof-error-p eof-value recursive-p)
        (%read-token))))

(declaim
 (ftype (function (&optional (or stream null)) (values string &optional))
        read-token))

(defun read-token
       (&optional stream &aux (*standard-input* (or stream *standard-input*)))
  (with-output-to-string (*standard-output*) (%read-token stream)))

(let ((pred (char-pred #\|)))
  (defun %read-token (&optional stream)
    (let ((*standard-input* (or stream *standard-input*)))
      (do-stream (char nil nil nil)
        (cond
          ((or (whitecharp char)
               ;; KLUDGE: for allegro.
               ;; allegro readtable is designed to every character has its reader-macro.
               ;; To ignore default behavior, named-readtables::%get-macro-character can be used.
               ;; But unfortunately, it does not return second value.
               ;; %get-macro-character is internal one
               ;; and its behavior satisfies named-readtables's needs enough.
               ;; We do not have the reason to change its behavior.
               ;; Fortunately non-terminating-p is used only for |#reader|.
               ;; And read-as-string does not have to need to extend.
               (and (named-readtables::%get-macro-character char *readtable*)
                    (not (char= char #\#))))
           (unread-char char)
           (return))
          ((char= #\\ char)
           (write-char char)
           (write-char (read-char)))
          ((char= #\| char)
           (write-char char)
           (do-stream-till (c pred nil t t)
             (write-char c)
             (when (char= #\\ c)
               (write-char (read-char)))))
          (t (write-char char)))))))

;;; MACRO CHARS

(declaim
 (ftype (function (stream character) (values null &optional))
        |"reader|
        |'reader|
        |paren-reader|
        |`reader|
        |;reader|
        |#reader|
        |,reader|))

(let ((pred (char-pred #\")))
  (defun |"reader| (stream character)
    (write-char character)
    (do-stream-till (c pred stream t t)
      (write-char c)
      (when (char= #\\ c)
        (write-char (read-char stream))))))

(defun |'reader| (stream character)
  (write-char character)
  (%read-as-string stream t t t))

(defun |paren-reader| (stream character)
  (let ((*print-pretty*)) ; For CLISP.
    (write-char character)
    (do-stream (char stream)
      (cond ;; end check
            ((char= #\) char)
             (write-char char)
             (return))
            ;; dotted list check.
            ((char= #\. char) (write-char char))
            ;; whitechar check.
            ((whitecharp char) (write-char char))
            ;; the default.
            (t
             (unread-char char)
             (%read-as-string stream t t t))))))

(defun |`reader| (stream character)
  (write-char character)
  (%read-as-string stream t t t))

(let ((pred (char-pred #\Newline)))
  (defun |;reader| (stream character)
    (write-char character)
    (handler-case
        (do-stream-till (c pred stream t t)
          (write-char c))
      (end-of-file ()))))

(defun |#reader| (stream character)
  (let* ((digit
          (parse-integer
            (read-string-till (lambda (c) (not (digit-char-p c))) stream)
            :junk-allowed t))
         (char (peek-char nil stream))
         (reader (get-dispatcher char)))
    (if reader
        (funcall (coerce reader 'function) stream (read-char stream) digit)
        (progn
         (unless *muffle-reader-error*
           (cerror
             (format nil "Anyway, read the notation with assigning true to ~S."
                     '*muffle-reader-error*)
             'no-dispatch-function
             :name char
             :stream stream)
           (setq *muffle-reader-error* t))
         (write-char character)
         (when digit
           (write digit))
         (write-char (read-char stream))
         (when (let ((next-char (peek-char nil stream nil nil)))
                 (and next-char
                      (not (or (whitecharp next-char) (char= #\) next-char)))))
           (%read-as-string stream t t t))))))

(defun |,reader| (stream character)
  (declare (ignore stream))
  (write-char character)
  nil)

;;;; READTABLE
;;; NOTE: In order to generate dispatch macro notation string, we need to use our own #-macro-char-reader.
;;; And we need our own dispatch macro character handling feature. (i.e. set-dispatcher, and get-dispatcher.)

(locally
 #+sbcl ; Out of our responsibilities.
 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
 (named-readtables:defreadtable as-string
   (:macro-char #\" '|"reader|)
   (:macro-char #\' '|'reader|)
   (:macro-char #\( '|paren-reader|)
   (:macro-char #\` '|`reader|)
   (:macro-char #\; '|;reader|)
   (:macro-char #\# '|#reader| :non-terminating)
   (:syntax-from :common-lisp #\) #\))
   (:macro-char #\, '|,reader|)))

(defvar *dispatch-macros* (make-hash-table :test #'equal))

(declaim
 (ftype (function (character (or symbol function) &optional readtable)
         (values (eql t) &optional))
        set-dispatcher))

(defun set-dispatcher (char fun &optional (readtable *readtable*))
  #+sbcl ; due to not base-char.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  #+(or clisp allegro abcl)
  (progn
   (check-type fun (or symbol function))
   (check-type readtable readtable))
  (setf (gethash
          (cons (char-upcase char) (named-readtables:readtable-name readtable))
          *dispatch-macros*)
          fun)
  t)

(declaim
 (ftype (function (character &optional readtable)
         (values (or null symbol function) &optional))
        get-dispatcher))

(defun get-dispatcher (char &optional (*readtable* *readtable*))
  #+sbcl ; due to not base-char.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  #+clisp
  (check-type *readtable* readtable)
  (let ((cons
         (cons (char-upcase char)
               (named-readtables:readtable-name *readtable*))))
    (declare (dynamic-extent cons))
    (values (gethash cons *dispatch-macros*))))

(let ((*readtable* (named-readtables:find-readtable *default-readtable*)))
  (set-dispatcher #\# '|##reader|)
  (set-dispatcher #\( '|#paren-reader|)
  (set-dispatcher #\= '|#=reader|)
  (set-dispatcher #\+ '|#+reader|)
  (set-dispatcher #\- '|#+reader|)
  (set-dispatcher #\* '|#=reader|)
  (set-dispatcher #\\ '|#\\reader|)
  (set-dispatcher #\' '|#=reader|)
  (set-dispatcher #\)
                  (get-dispatch-macro-character #\# #\) (copy-readtable nil)))
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
  (set-dispatcher #\X '|#=reader|))

;;; DISPATCH MACRO CHARS

(defun |##reader| (stream character number)
  (declare (ignore stream))
  (write-char #\#)
  (when number
    (write number))
  (write-char character)
  nil)

(defun |#paren-reader| (stream character number)
  (write-char #\#)
  (when number
    (write number))
  (|paren-reader| stream character))

(defun |#=reader| (stream character number)
  (write-char #\#)
  (when number
    (write number))
  (write-char character)
  (%read-as-string stream t t t))

(defun |#\|reader| (stream character number)
  (funcall (formatter "#~@[~D~]~C") *standard-output* number character)
  (do-stream (char stream)
    (case char
      (#\|
       (write-char char)
       (when (char= #\# (peek-char nil stream))
         (write-char (read-char stream))
         (return)))
      (#\#
       (case (peek-char nil stream)
         (#\| ; nested comment
          (|#\|reader| stream (read-char stream) nil))
         ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
          (let ((digit (read-string-till (lambda (c) (not (digit-char-p c)))))
                (c (peek-char nil stream)))
            (if (char= #\| c) ; nested comment with digit.
                (|#\|reader| stream (read-char stream) digit)
                (progn (write-char char) (write-string digit)))))
         (otherwise (write-char char))))
      (otherwise (write-char char)))))

(defun |#+reader| (stream character number)
  (write-char #\#)
  (when number
    (write number))
  (write-char character)
  (%read-as-string stream t t t)
  (%read-as-string stream t t t))

(let ((pred (char-pred #\>)))
  (defun |#<reader| (stream character number)
    (unless *muffle-reader-error*
      (cerror
        (format nil "Anyway, read the notation with assigning true to ~S."
                '*muffle-reader-error*)
        'read-unreadable-object
        :stream stream)
      (setq *muffle-reader-error* t))
    (write-char #\#)
    (when number
      (write number))
    (write-char character)
    (do-stream-till (c pred stream t t)
      (write-char c)
      (when (char= #\\ c)
        (write-char (read-char stream))))))

(defun |#\\reader| (stream character number)
  (unread-char character stream)
  (write-char #\#)
  (when number
    (write number))
  (%read-token stream))