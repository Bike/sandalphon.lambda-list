(in-package #:sandalphon.lambda-list)

(defun parse-lambda-list (lambda-list grammar-spec)
  "Given a lambda list, and a grammar specification, returns a LAMBDA-LIST object.
A grammar specification is a list of clause specifications. A clause specification is either a class specifier, or a list (class-specifier &key).
The grammar specification is simply based on clauses earlier in the list having to occur before clauses later in the list. All clauses are considered optional. 
The keys and values in the list are passed unevaluated to MAKE-INSTANCE.
Certain keys have special meanings to the grammar itself. These are removed before they can be passed to MAKE-INSTANCE. They are as follows:
* :DATA-DESTRUCTURE indicates that a dotted list indicates this kind of clause. E.g., in parsing (foo bar . baz), if there was a &rest clause in the grammar with :DATA-DESTRUCTURE, the result would be the same as from (foo bar &rest baz).
* :ANYWHERE indicates that the position of this clause in the grammar is unimportant, and that this clause can be anywhere in the lambda list, except, for bad reasons, before the first clause. This is used for &environment clauses, which cannot be before &whole.

EXAMPLE: A lambda-list like an ordinary lambda-list but with keys having :no-value as a default if no default is explicitly given would have the grammar specification (REGULAR-CLAUSE OPTIONAL-CLAUSE REST-CLAUSE (KEY-CLAUSE :DEFAULT-DEFAULT :NO-VALUE) AUX-CLAUSE)."
  (%parse-lambda-list lambda-list (make-grammar grammar-spec)))

(defgeneric clause-parse (clause list lambda-list)
  (:documentation "Parse an entire clause. Side-effect the results into CLAUSE, and return a list with the clause consumed. For example, given (&whole foo bar baz), the &whole clause parser would return (bar baz)."))
(defgeneric multiple-clause-parse (clause spec lambda-list)
  (:documentation "Parse an individual specifier for a multiple-clause. The return format is particular to what the particular clause class expects."))

(defun grammar-clean-keywords (list)
  "Takes a plist of key/values, cleans out grammar specials, and returns something that should be fine for MAKE-INSTANCE and friends.
Specials are presently :DATA-DESTRUCTURE, :ANYWHERE."
  ;; but it's easy to add any, at least for this function
  (let ((to-kill '(:data-destructure :anywhere)))
    (loop for (key value) on list by #'cddr
       when (find key to-kill)
       collect key into kws
       else collect key into clean and collect value into clean
       finally (return (values clean kws)))))

(defun make-grammar (grammar-spec)
  "Takes a grammar specification and makes it into a fresh \"grammar\", which is an internal thing passed to %parse-lambda-list that is full of clauses subject to side ffects."
  (loop with blank = nil
     with anywhere = nil
     with data-destructure = nil
     for thing in grammar-spec
     collecting
       (multiple-value-bind (class cleaned kws)
	   (if (listp thing)
	       (multiple-value-call #'values (first thing)
		       (grammar-clean-keywords (rest thing)))
	       (values thing nil nil))
	 (let ((obj (apply #'make-instance class cleaned)))
	   (when (find :data-destructure kws)
	     (if data-destructure
		 (error "grammar spec ~a has multiple clauses with ~a"
			grammar-spec :data-destructure)
		 (setf data-destructure t)))
	   (when (find :anywhere kws)
	     (if anywhere
		 (error "grammar spec ~a has multiple clauses with ~a"
			grammar-spec :anywhere)
		 (setf anywhere t)))
	   (when (null (clause-keywords obj))
	     (if blank
		 (error
		  "grammar spec ~a has multiple clauses with no keyword"
		  grammar-spec)
		 (setf blank t)))
	   (list (clause-keywords obj) obj kws)))))

(defun %parse-lambda-list (llist grammar)
  ;; grammar format = list of (ll-keywords object keywords)
  ;; keywords right now being :anywhere and :data-destructure
  (loop with ret = (make-instance 'lambda-list
				  :clauses (mapcar #'second grammar))
     with last = nil
     with been-anywhere = nil
     with working = llist
     do (flet ((find-clause (kw)
		 (loop for spec in grammar
		    do (when (member kw (first spec))
			 (if (find :anywhere (third spec))
			     (if been-anywhere
				 (error "Multiple ~a clauses"
					(first spec))
				 (progn
				   ;; &environment can only be after
				   ;;  &whole. silly. KLUDGE
				   (when (null last)
				     (setf last (first grammar)))
				   (return spec)))
			     (if (or (null last)
				      ;; if this spec is after
				      ;;  the last spec.
				      (find
				       spec
				       (rest (member last grammar))))
				 ;; we're in a proper position,
				 ;;  return the spec
				 (return (setf last spec))
				 ;; fail
				 (error "~a clause in wrong position"
					(first spec))))))))
	  (etypecase working
	    (null (loop-finish))
	    (symbol
	     (let ((dd (find :data-destructure grammar
			     :key #'third :test #'find)))
	       (cond ((not dd)
		      (error "Improperly dotted lambda list"))
		     ((not (find dd (rest (member last grammar))))
		      (error "Dot in lambda list not allowed in this position"))
		     (t
		      (setf working
			    (clause-parse dd (list working) ret))
		      (loop-finish)))))
	    ((cons null) (error "Can't name a parameter NIL"))
	    ((cons symbol)
	     (let ((spec (find-clause (first working))))
	       (if spec
		   (setf working
			 (clause-parse (second spec) working ret))
		   (let ((blank (find-clause nil)))
		     (cond
		       (blank
			(setf working
			      (clause-parse (second blank)
					    working ret)))
		       (t (error "Parameter ~a outside of clause"
				 (first working))))))))
	    ((cons t) ; a destructure, probably
	     (let ((blank (find-clause nil)))
	       (cond (blank
		      (setf working
			    (clause-parse (second blank) working ret)))
		     (t (error "Parameter ~a outside of clause"
			       (first working))))))))
     finally (return ret)))

;;; What follows are the parse methods for standard clauses.

(defun parse-destructure (llist grammar-name)
  "Helper function for destructuring parameters. Given a maybe-lambda list and the name of a grammar, parses."
  (if (symbolp llist)
      llist
      (%parse-lambda-list llist
			  (make-grammar (symbol-value grammar-name)))))

(declaim (inline make-key))
(defun make-key (symbol)
  "For &key parsing."
  (intern (symbol-name symbol) "KEYWORD"))

(defmethod clause-parse ((clause singleton-clause) list llist)
  (declare (ignore llist)) ; FIXME better errors
  (let ((spec (second list)))
    (unless (symbolp spec)
      (error "~a parameter is not a symbol" (clause-keywords clause)))
    (setf (clause-spec clause) spec)
    (cddr list)))

(defmethod clause-parse ((clause multiple-clause) list llist)
  (loop with specs = nil
     for list on list
     do (let* ((thing (first list))
	       (res (multiple-clause-parse clause thing llist)))
	  (cond ((null res) ; thing is not part of this clause
		 (setf (multiple-clause-specs clause)
		       (nreverse specs))
		 (return list))
		(t (push res specs))))
     finally (setf (multiple-clause-specs clause)
		   (nreverse specs))))

(defmethod clause-parse ((clause optional-clause) list llist)
  ;; skip &optional
  (call-next-method clause (rest list) llist))

(defmethod clause-parse ((clause destructuring-optional-clause)
			 list llist)
  ;; skip &optional
  (call-next-method clause (rest list) llist))

(defmethod clause-parse ((clause key-clause) list llist)
  ;; special due to aok
  (when (eq (first list) '&allow-other-keys)
    (error "~a without ~a" '&allow-other-keys '&key))
  (loop with specs = nil
     for list on (rest list)
     do (let* ((thing (first list)) (rest (rest list)))
	  (when (eq thing '&allow-other-keys)
	    (setf (key-clause-aok-p clause) t
		  (multiple-clause-specs clause) (nreverse specs))
	    (return rest))
	  (let ((res (multiple-clause-parse clause thing llist)))
	    (cond ((null res)
		   (setf (multiple-clause-specs clause)
			 (nreverse specs))
		   (return list))
		  (t (push res specs)))))
     finally (setf (multiple-clause-specs clause)
		   (nreverse specs))))

(defmethod clause-parse ((clause destructuring-key-clause) list llist)
  ;; special due to aok
  (when (eq (first list) '&allow-other-keys)
    (error "~a without ~a" '&allow-other-keys '&key))
  (loop with specs = nil
     for list on (rest list)
     do (let* ((thing (first list)) (rest (rest list)))
	  (when (eq thing '&allow-other-keys)
	    (setf (key-clause-aok-p clause) t
		  (multiple-clause-specs clause) (nreverse specs))
	    (return rest))
	  (let ((res (multiple-clause-parse clause thing llist)))
	    (cond ((null res)
		   (setf (multiple-clause-specs clause)
			 (nreverse (cons res specs)))
		   (return list))
		  (t (push res specs)))))
     finally (setf (multiple-clause-specs clause)
		   (nreverse specs))))

(defmethod clause-parse ((clause aux-clause) list llist)
  ;; skip &aux
  (call-next-method clause (rest list) llist))

(defmethod multiple-clause-parse ((clause regular-clause) spec llist)
  (cond ((find spec (lambda-list-keywords llist)) nil)
	((symbolp spec) spec)
	(t (error "~a is not a suitable parameter" spec))))

(defmethod multiple-clause-parse ((clause specialized-regular-clause)
				  spec llist)
  (cond ((find spec (lambda-list-keywords llist)) nil)
	((symbolp spec) (list spec 't))
	((and (consp spec) (consp (cdr spec)) (null (cddr spec)))
	 spec)
	(t (error "~a is not a suitable parameter" spec))))

(defmethod multiple-clause-parse ((clause destructuring-regular-clause)
				  spec llist)
  (cond ((not (symbolp spec))
	 (parse-destructure spec (destructuring-clause-grammar clause)))
	((find spec (lambda-list-keywords llist)) nil)
	(t spec)))

(defmethod multiple-clause-parse ((clause optional-clause) spec llist)
  (cond ((find spec (lambda-list-keywords llist)) nil)
	((listp spec)
	 (destructuring-bind (var
			      &optional
			      (default (clause-default-default clause))
			      -p)
	     spec
	   (unless (symbolp var)
	     (error "~a is not a valid &optional parameter name" var))
	   (list var default -p)))
	((symbolp spec) (list spec nil nil))
	(t (error "~a is not a valid &optional spec" spec))))

(defmethod multiple-clause-parse ((clause destructuring-optional-clause)
				  spec llist)
  (cond ((listp spec)
	 (destructuring-bind (var
			      &optional
			      (default (clause-default-default clause))
			      -p)
	     spec
	   (list (parse-destructure
		  var (destructuring-clause-grammar clause))
		 default -p)))
	((not (symbolp spec)) (error "~a is not a valid &optional spec"
				     spec))
	((find spec (lambda-list-keywords llist)) nil)
	(t (list spec (clause-default-default clause) nil))))

(defmethod multiple-clause-parse ((clause key-clause) spec llist)
  (cond ((find spec (lambda-list-keywords llist)) nil)
	((symbolp spec)
	 (list (list (make-key spec) spec)
	       (clause-default-default clause)
	       nil))
	((not (listp spec))
	 (error "~a is not a valid &key spec" spec))
	((listp (first spec))
	 (destructuring-bind ((key var) &optional
			      (default (clause-default-default clause))
			      -p)
	     spec
	   (unless (symbolp var)
	     (error "~a is not a valid &key name" var))
	   (list (list key var) default -p)))
	(t (destructuring-bind (var
				&optional
				(default
				    (clause-default-default clause))
				-p)
	       spec
	     (unless (symbolp var)
	       (error "~a is not a valid &key name" var))
	     (list (list (make-key var) var) default -p)))))

(defmethod multiple-clause-parse ((clause destructuring-key-clause)
				  spec llist)
  (cond ((find spec (lambda-list-keywords llist)) nil)
	((symbolp spec)
	 (list (list (make-key spec) spec)
	       (clause-default-default clause)
	       nil))
	((not (listp spec))
	 (error "~a is not a valid &key spec" spec))
	((listp (first spec))
	 (destructuring-bind ((key var) &optional
			      (default (clause-default-default clause))
			      -p)
	     spec
	   ;; this is the only case that can actually destructure.
	   (list (list key (parse-destructure
			    var (destructuring-clause-grammar clause))
		       default -p))))
	(t (destructuring-bind (var
				&optional
				(default
				    (clause-default-default clause))
				-p)
	       spec
	     (list (list (make-key var) var) default -p)))))

(defmethod multiple-clause-parse ((clause aux-clause) spec llist)
  (cond ((find spec (lambda-list-keywords llist)) nil)
	((symbolp spec) (list spec (clause-default-default clause)))
	((listp spec)
	 (destructuring-bind (var
			      &optional
			      (default (clause-default-default clause)))
	     spec
	   (list var default)))
	(t (error "~a is not a valid ~a parameter spec"
		  spec '&aux))))
