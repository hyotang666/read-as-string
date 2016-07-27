(in-package :cl-user)
(defpackage :ras.bsearch(:use :cl :ras.utility)
  (:export #:bsearch))
(in-package :ras.bsearch)

(prototype bsearch (T simple-vector
		      &key
		      (:key function)
		      (:test function)
		      (:start (mod #.array-total-size-limit))
		      (:end (mod #.array-total-size-limit))
		      (:compare function)
		      (:default T))
	   (values (or string T)))

#++(defun bsearch(item vector &key(key #'identity)(test #'eql)(start 0)(end (length vector))(compare #'<)(default nil))
  #.(doc :ras.bsearch "doc/bsearch.md")
  (multiple-value-bind(center %)(floor (- end start) 2)
    (if(zerop center) ; Reached binary tree's end.
      (if(zerop %) ; (= end start).
	(let((target(svref vector (+ center start 1))))
	  (if(funcall test item(funcall key target))
	    target
	    default))
	(let((target(svref vector (+ center start))))
	  (if(funcall test item(funcall key target))
	    target
	    default)))
      (let*((index(+ center start))
	    (target(svref vector index))
	    (elt(funcall key target)))
	(if(funcall test item elt)
	  target
	  (if(funcall compare item elt)
	    (bsearch item vector :start start :end index :test test :key key :compare compare :default default)
	    (bsearch item vector :start index :end end :test test :key key :compare compare :default default)))))))

(defun bsearch(item vector &key(key #'identity)(test #'eql)(start 0)(end (length vector))(compare #'<)(default nil))
  #.(doc :ras.bsearch "doc/bsearch.md")
  (labels((on-the-node(center %)
	    (if(zerop center)
	      (reached-leaf % (+ center start))
	      (check-contents(+ center start))))
	  (reached-leaf(% index)
	    (determine-return-value(svref vector (if(zerop %) ; (= end start)
						   (1+ index)
						   index))))
	  (determine-return-value(target)
	    (if(funcall test item(funcall key target))
	      target
	      default))
	  (check-contents(index)
	    (let*((target(svref vector index))
		  (elt(funcall key target)))
	      (if(funcall test item elt)
		target
		(rec elt index))))
	  (rec(elt index)
	    (if(funcall compare item elt)
	      (bsearch item vector :start start :end index :test test :key key :compare compare :default default)
	      (bsearch item vector :start index :end end :test test :key key :compare compare :default default))))
    (declare(inline reached-leaf determine-return-value check-contents rec))
    (multiple-value-call #'on-the-node (floor (- end start) 2))))
