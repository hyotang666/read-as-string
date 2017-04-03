(in-package :cl-user)
(defpackage :ras.bsearch(:use :cl)
  (:import-from :type-ext #:Prototype)
  (:export #:bsearch))
(in-package :ras.bsearch)

(Prototype bsearch (T simple-vector
		      &key
		      (:key function)
		      (:test function)
		      (:start (mod #.array-total-size-limit))
		      (:end (mod #.array-total-size-limit))
		      (:compare function)
		      (:default T))
	   (values (or string T)))

#++(defun bsearch(item vector &key(key #'identity)(test #'eql)(start 0)(end (length vector))(compare #'<)(default nil))
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
  (labels((ON-THE-NODE(center %)
	    (if(zerop center)
	      (REACHED-LEAF % (+ center start))
	      (CHECK-CONTENTS(+ center start))))
	  (REACHED-LEAF(% index)
	    (DETERMINE-RETURN-VALUE(svref vector (if(zerop %) ; (= end start)
						   (1+ index)
						   index))))
	  (DETERMINE-RETURN-VALUE(target)
	    (if(funcall test item(funcall key target))
	      target
	      default))
	  (CHECK-CONTENTS(index)
	    (let*((target(svref vector index))
		  (elt(funcall key target)))
	      (if(funcall test item elt)
		target
		(REC elt index))))
	  (REC(elt index)
	    (if(funcall compare item elt)
	      (bsearch item vector :start start :end index :test test :key key :compare compare :default default)
	      (bsearch item vector :start index :end end :test test :key key :compare compare :default default))))
    (declare(inline REACHED-LEAF DETERMINE-RETURN-VALUE CHECK-CONTENTS REC))
    (multiple-value-call #'ON-THE-NODE (floor (- end start) 2))))
