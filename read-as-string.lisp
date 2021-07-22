(in-package :cl-user)

(defpackage :read-as-string
  (:use :cl :core-reader)
  (:export ;; main api.
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
           #:read-token))

(in-package :read-as-string)

(pushnew :read-as-string *features*)

(declaim (optimize speed))

;;;; VARIABLES

(defvar *muffle-reader-error* nil)

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

(declaim
 (ftype (function (character) (values (or null character) &optional))
        whitecharp))

(setf (symbol-function 'whitecharp)
        (delimiter '(#\Space #\Newline #\Tab #\Page #\Return #\Newline)))

;;;; READ-AS-STRING

(declaim
 (ftype (function (&optional (or stream null) boolean t boolean)
         (values (or string t) &optional))
        read-as-string))

(defun read-as-string
       (&optional stream (eof-error-p t) (eof-value nil) (recursive-p nil))
  (flet ((may-peek ()
           (handler-case (peek-char (null recursive-p) nil t t recursive-p)
             (end-of-file (c)
               (if eof-error-p
                   (error c)
                   (return-from read-as-string eof-value))))))
    (let* ((*readtable* (named-readtables:find-readtable 'as-string))
           (*standard-input* (or stream *standard-input*))
           (char (may-peek)))
      (multiple-value-call #'concatenate
        'string
        (if recursive-p
            (read-string-till (lambda (c) (not (whitecharp c))))
            "")
        (if (get-macro-character
              (if recursive-p
                  (may-peek)
                  char))
            (read *standard-input* eof-error-p eof-value recursive-p)
            (read-token))))))

(declaim
 (ftype (function (&optional (or stream null)) (values string &optional))
        read-token))

(defun read-token
       (&optional stream &aux (*standard-input* (or stream *standard-input*)))
  (with-output-to-string (*standard-output*)
    (loop :for char := (read-char nil nil nil nil)
          :while char
          :if (or (whitecharp char)
                  (multiple-value-bind (macro non-terminal-p)
                      (get-macro-character char)
                    (and macro (not non-terminal-p))))
            :do (unread-char char)
                (loop-finish)
          :else :if (char= #\\ char)
            :do (write-char char)
                (write-char (read-char))
          :else :if (char= #\| char)
            :do (write-char char)
                (do-stream-till (c (lambda (c) (char= #\| c)) nil t t)
                  (write-char c))
          :else
            :do (write-char char))))

;;; MACRO CHARS

(declaim
 (ftype (function (stream character) (values simple-string &optional))
        |"reader|
        |'reader|
        |paren-reader|
        |`reader|
        |;reader|
        |#reader|
        |,reader|))

(defun |"reader| (stream character)
  (prin1-to-string
    (funcall
      (load-time-value
       (coerce (get-macro-character #\" (copy-readtable nil)) 'function) t)
      stream character)))

(defun |'reader| (stream character)
  (format nil "~C~A" character (read-as-string stream t t t)))

(defun |paren-reader| (stream character)
  (declare (ignore character))
  (let ((*print-pretty*)) ; For CLISP.
    (format nil "(~{~A~}"
            (loop :for char = (peek-char nil stream)
                  ;; end check.
                  :if (char= #\) char)
                    :collect (read-char stream)
                    :and :do (loop-finish)
                  ;; dotted list check.
                  :else :if (char= #\. char)
                    :collect (read-char stream)
                  ;; Whitechar check.
                  :else :if (whitecharp char)
                    :collect (read-char stream)
                  ;; The default.
                  :else
                    :collect (read-as-string stream t t t)))))

(defun |`reader| (stream character)
  (format nil "~C~A" character (read-as-string stream t t t)))

(defun |;reader| (stream character)
  (concatenate 'string (string character)
               (read-string-till (char-pred #\Newline) stream t t t t)))

(defun |#reader| (stream character)
  (let* ((digit
          (parse-integer
            (read-string-till (lambda (c) (not (digit-char-p c))) stream)
            :junk-allowed t))
         (char (peek-char nil stream))
         (reader (get-dispatcher char)))
    (if reader
        (funcall (coerce reader 'function) stream (read-char stream) digit)
        (if *muffle-reader-error*
            (format nil "~C~@[~D~]~C~@[~A~]" character digit (read-char stream)
                    (when (let ((next-char (peek-char nil stream nil nil)))
                            (and next-char
                                 (not
                                   (or (whitecharp next-char)
                                       (char= #\) next-char)))))
                      (read-as-string stream t t t)))
            (error 'no-dispatch-function :name char :stream stream)))))

(defun |,reader| (stream character)
  (declare (ignore stream)
           (type character character))
  (string character))

;;;; READTABLE

(locally
 (declare (optimize (speed 1)))
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
  (declare (optimize (speed 1)))
  #+clisp
  (progn
   (check-type fun (or symbol function))
   (check-type readtable readtable))
  (setf (gethash (cons (char-upcase char) readtable) *dispatch-macros*) fun)
  t)

(declaim
 (ftype (function (character &optional readtable)
         (values (or null symbol function) &optional))
        get-dispatcher))

(defun get-dispatcher (char &optional (*readtable* *readtable*))
  (declare (optimize (speed 1)))
  #+clisp
  (check-type *readtable* readtable)
  (values (gethash (cons (char-upcase char) *readtable*) *dispatch-macros*)))

(let ((*readtable* (named-readtables:find-readtable 'as-string)))
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
  (format nil "#~@[~D~]~C" number character))

(defun |#paren-reader| (stream character number)
  (format nil "#~@[~D~]~A" number (|paren-reader| stream character)))

(defun |#=reader| (stream character number)
  (format nil "#~@[~D~]~C~A" number character (read-as-string stream t t t)))

(defun |#\|reader| (stream character number)
  (with-output-to-string (out)
    (funcall (formatter "#~@[~D~]~C") out number character)
    (loop :for char = (read-char stream)
          :do (case char
                (#\|
                 (write-char char out)
                 (when (char= #\# (peek-char nil stream))
                   (write-char (read-char stream) out)
                   (loop-finish)))
                (#\#
                 (case (peek-char nil stream)
                   (#\| ; nested comment
                    (write-string (|#\|reader| stream (read-char stream) nil)
                                  out))
                   ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
                    (let ((digit
                           (read-string-till
                             (lambda (c) (not (digit-char-p c)))))
                          (c (peek-char nil stream)))
                      (if (char= #\| c) ; nested comment with digit.
                          (write-string
                            (|#\|reader| stream (read-char stream) digit) out)
                          (progn
                           (write-char char out)
                           (write-string digit out)))))
                   (otherwise (write-char char out))))
                (otherwise (write-char char out))))))

(defun |#+reader| (stream character number)
  (format nil "#~@[~D~]~C~A~A" number character (read-as-string stream t t t)
          (read-as-string stream t t t)))

(defun |#<reader| (stream character number)
  (if *muffle-reader-error*
      (format nil "#~@[~D~]~A" number
              (read-delimited-string #\> stream character))
      (error 'read-unreadable-object :stream stream)))

(defun |#\\reader| (stream character number)
  (unread-char character stream)
  (format nil "#~@[~D~]~A" number (read-token stream)))
