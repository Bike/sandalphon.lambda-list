(in-package #:sandalphon.lambda-list)

;;;; BINDS

(defun gen-binds (llist &rest forms)
  (loop with safe = (if (lambda-list-safe llist)
			(gensym "SAFETY")
			nil)
     with binds = (if safe
		      `((,safe ,(length-check llist (first forms))))
		      nil)
     with declares = (if safe `((ignore ,safe)) nil)
     for clause in (lambda-list-clauses llist)
     do (multiple-value-bind (new-binds new-forms new-declares)
	    (clause-binds clause forms)
	  (setf binds (append binds new-binds)
		forms new-forms
		declares (append declares new-declares)))
     finally (return (values binds declares))))

;; return (values binds new-forms declares)
(defgeneric clause-binds (clause forms))
(defgeneric multiple-clause-binds (clause spec forms))

(defmethod clause-binds ((clause whole-clause) forms)
  ;; also handles macro CDR/cmf CDR-FUNCALL-THINGIE.
  ;; you know. the rest of the lambda list handling some other thing.
  (let ((mapped (gensym "MAPPED")))
    (values (append (when (clause-spec clause)
		      `((,(clause-spec clause) ,(first forms))))
		    `((,mapped (,(whole-map clause) ,(first forms)))))
	    (cons mapped (rest forms))
	    `((ignorable ,mapped)))))

(defmethod clause-binds ((clause environment-clause) forms)
  (values `((,(clause-spec clause) ,(second forms)))) forms nil)

(defmethod clause-binds ((clause multiple-clause) forms)
  (loop with binds = nil
     with declares = nil
     for spec in (multiple-clause-specs clause)
     do (multiple-value-bind (new-binds new-forms new-declares)
	    (multiple-clause-binds clause spec forms)
	  (setf binds (append binds new-binds)
		forms new-forms
		declares (append declares new-declares)))
     finally (return (values binds forms declares))))

(defmethod clause-binds ((clause key-clause) forms)
  (let ((keys (gensym "KEYS")))
    ;; c-n-m does multiple-clause
    (multiple-value-bind (binds nforms decls)
	(call-next-method clause (cons keys (rest forms)))
      (values (list* `(,keys ,(first forms)) binds) nforms decls))))

(defun regular-binds (name form)
  `((,name (car ,form))))

(defmethod multiple-clause-binds ((clause regular-clause) spec forms)
  (values (regular-binds spec (first forms))
	  (cons (list 'cdr (first forms)) (rest forms))
	  nil))

(defmethod multiple-clause-binds ((clause specialized-regular-clause)
				  spec forms)
  (values (regular-binds (first spec) (first forms))
	  (cons (list 'cdr (first forms)) (rest forms))
	  `((type ,(second spec) ,(first spec)))))

(defmethod multiple-clause-binds ((clause destructuring-regular-clause)
				  spec forms)
  (if (symbolp spec)
      (values (regular-binds spec (first forms))
	      (cons (list 'cdr (first forms)) (rest forms))
	      nil)
      (let ((sym (gensym "DESTRUCTURE")))
	(multiple-value-bind (r-binds r-decls)
	    (apply #'gen-binds spec sym (rest forms))
	  (values
	   (append (regular-binds sym (first forms)) r-binds)
	   (cons (list 'cdr (first forms)) (rest forms))
	   r-decls)))))

(defun optional-binds (var -p default form)
  `((,-p (if (null ,form) nil ,form))
    (,var (if ,-p (car ,-p) ,default))))

(defmethod multiple-clause-binds ((clause optional-clause) spec forms)
  (values
   (optional-binds (first spec) (or (third spec) (gensym "PROVIDED-P"))
		   (second spec) (first forms))
   (cons (list 'cdr (first forms)) (rest forms))
   nil))

(defmethod multiple-clause-binds ((clause destructuring-optional-clause)
				  spec forms)
  (destructuring-bind (var default -p) spec
    (if (symbolp var)
	(values (optional-binds var (or -p (gensym "PROVIDED-P"))
				default (first forms))
		(cons (list 'cdr (first forms)) (rest forms))
		nil)
	(let ((sym (gensym "DESTRUCTURE")))
	  (multiple-value-bind (r-binds r-decls)
	      (apply #'gen-binds var sym (rest forms))
	    (values
	     (append (optional-binds sym (or -p (gensym "PROVIDED-P"))
				     default (first forms))
		     r-binds)
	     (cons (list 'cdr (first forms)) (rest forms))
	     r-decls))))))

(defmethod clause-binds ((clause rest-clause) forms)
  (if (clause-spec clause)
      (values `((,(clause-spec clause) ,(first forms)))
	      (list (clause-spec clause) (rest forms))
	      nil)
      (values nil forms nil)))

(defun key-binds (key var -p default form)
  (let ((not-found-tag (gensym))) ; gross
    `((,-p (getf ,form ',key ',not-found-tag))
      (,var (if (eq ,-p ',not-found-tag)
		,default
		,-p)))))

(defmethod multiple-clause-binds ((clause key-clause) spec forms)
  (destructuring-bind ((key var) default -p) spec
    (values (key-binds key var (or -p (gensym "PROVIDED-P"))
		       default (first forms))
	    forms
	    nil)))

(defmethod multiple-clause-binds ((clause destructuring-key-clause)
				  spec forms)
  (destructuring-bind ((key var) default -p) spec
    (let ((-p (or -p (gensym "PROVIDED-P"))))
      (if (symbolp var)
	  (values (key-binds key var -p default (first forms))
		  (cons (list 'cdr (first forms)) (rest forms))
		  nil)
	  (let ((sym (gensym "DESTRUCTURE")))
	    (multiple-value-bind (r-binds r-decls)
		(apply #'gen-binds var sym (rest forms))
	      (values
	       (append (key-binds key sym -p default (first forms))
		       r-binds)
	       (cons (list 'cdr (first forms)) (rest forms))
	       r-decls)))))))

(defmethod multiple-clause-binds ((clause aux-clause) spec forms)
  (values (list spec) forms nil))
