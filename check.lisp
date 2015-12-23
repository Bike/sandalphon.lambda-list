(in-package #:sandalphon.lambda-list)

;;;; (Hopefully) little functions for checking lambda list stuff
;;;;  in safe code. Too many arguments is the main one, but I'll throw
;;;;  in keywords (less little) here while I'm at it.

;;; keyword stuff

(defun verify-keys (list keys aok-p)
  (loop with bad-keys = nil
     with seen-aok = nil ; whether :allow-other-keys has been seen
     do (typecase list
	  (null (loop-finish))
	  (cons (typecase (cdr list)
		  (null
		   (error "Odd number of arguments in &key plist: ~a" 
			  list))
		  ((not cons) (error "Dotted list as &key plist ~a"
				     list))
		  (t (cond (aok-p (setf list (cddr list)))
			   (t (destructuring-bind (key value &rest _)
				  list
				(cond
				  ((eq key :allow-other-keys)
				   (unless seen-aok
				     (setf seen-aok t
					   aok-p (or aok-p value))))
				  ((find key keys) nil)
				  (t (push key bad-keys)))
				(setf list _))))))))
     finally (when bad-keys
	       (error "Unknown keys ~a: ~a expected"
		      bad-keys (cons :allow-other-keys keys)))))

;;; length stuff

(defun hazardous-list-length (list)
  "LIST-LENGTH but returns NIL for dotted lists."
  (typecase list
    (null 0)
    (cons (do ((n 0 (+ n 2))
	       (fast list (cddr fast))
	       (slow list (cdr slow)))
	      ()
	    (typecase fast
	      (null (return n))
	      ((not cons) (return nil)))
	    (typecase (cdr fast)
	      (null (return (1+ n)))
	      ((not cons) (return nil)))
	    (when (and (eq fast slow) (> n 0))
	      (return nil))))
    (t nil)))

(defun length-check (lambda-list form)
  (multiple-value-bind (min max)
      (lambda-list-args-accepted lambda-list)
    (let ((length (gensym "LENGTH")))
      `(let ((,length (hazardous-list-length ,form)))
	 ,(if max
	      `(cond ((not ,length)
		      (error "Dotted or circular list"))
		     ((> ,length ,min)
		      (error "Too many arguments"))
		     ((< ,max ,length)
		      (error "Not enough arguments")))
	      `(when (> ,min ,length)
		 (error "Not enough arguments")))))))

(defun lambda-list-args-accepted (lambda-list)
  (loop with min = 0 with max = 0
     for clause in (lambda-list-clauses lambda-list)
     do (multiple-value-bind (cmin cmax)
	    (clause-args-accepted clause)
	  (incf min cmin)
	  (if (or (null max) (null cmax))
	      (setf max nil)
	      (incf max cmax)))
     finally (return (values min max))))

(defgeneric clause-args-accepted (clause)
  (:documentation "Returns (VALUES MIN MAX), where MIN and MAX are of arguments consumed by this clause. A MAX of NIL represents infinity."))

(defmethod clause-args-accepted ((clause regular-clause))
  (let ((len (length (multiple-clause-specs clause))))
    (values len len)))
(defmethod clause-args-accepted ((clause optional-clause))
  (values 0 (length (multiple-clause-specs clause))))
(defmethod clause-args-accepted ((clause rest-clause)) (values 0 nil))
(defmethod clause-args-accepted ((clause key-clause)) (values 0 nil))
(defmethod clause-args-accepted ((clause aux-clause)) (values 0 0))

(defmethod clause-args-accepted ((clause specialized-regular-clause))
  (let ((len (length (multiple-clause-specs clause))))
    (values len len)))

;; these two are irrelevant to calculations, so
(defmethod clause-args-accepted ((clause whole-clause)) (values 0 0))
(defmethod clause-args-accepted ((clause environment-clause))
  (values 0 0))

(defmethod clause-args-accepted ((clause destructuring-regular-clause))
  (let ((len (length (multiple-clause-specs clause))))
    (values len len)))
(defmethod clause-args-accepted
    ((clause destructuring-optional-clause))
  (values 0 (length (multiple-clause-specs clause))))
(defmethod clause-args-accepted ((clause destructuring-key-clause))
  (values 0 nil))
