(in-package #:sandalphon.lambda-list)

(defmethod print-object ((o lambda-list) stream)
  (print-unreadable-object (o stream :type t)
    (write (lambda-list-unparse o) :stream stream)))

(defgeneric lambda-list-unparse (lambda-list)
  (:method ((lambda-list lambda-list))
    (loop for clause in (lambda-list-clauses lambda-list)
       appending (clause-unparse clause))))

(defgeneric clause-unparse (clause))
(defgeneric multiple-clause-unparse-spec (clause spec))

;;; standard unparsers

(defmethod clause-unparse ((clause singleton-clause))
  (if (clause-spec clause)
      (list (first (clause-keywords clause)) (clause-spec clause))
      nil))

(defmethod clause-unparse ((clause multiple-clause))
  (flet ((unparse (spec)
	   (multiple-clause-unparse-spec clause spec)))
    (cond ((null (multiple-clause-specs clause)) nil)
	  ((member nil (clause-keywords clause))
	   (mapcar #'unparse (multiple-clause-specs clause)))
	  (t (list* (first (clause-keywords clause))
		    (mapcar #'unparse
			    (multiple-clause-specs clause)))))))

(defmethod multiple-clause-unparse-spec ((clause regular-clause) spec)
  spec)
(defmethod multiple-clause-unparse-spec
    ((clause specialized-regular-clause) spec)
  spec)
(defmethod multiple-clause-unparse-spec ((clause optional-clause) spec)
  (if (third spec)
      spec
      ;; always display the default; we could check the default-default
      ;;  but that's work
      (list (first spec) (second spec))))
(defmethod clause-unparse ((clause key-clause))
  (let ((main (call-next-method)))
    (if (key-clause-aok-p clause)
	(append main '(&allow-other-keys))
	main)))
(defmethod multiple-clause-unparse-spec ((clause key-clause) spec)
  (let ((first (if (string= (symbol-name (first (first spec)))
			    (symbol-name (second (first spec))))
		   (second (first spec))
		   (first spec))))
    (if (third spec)
	(list* first (rest spec))
	;; ditto &optional
	(list first (second spec)))))
(defmethod multiple-clause-unparse-spec ((clause aux-clause) spec)
  spec)

(defun maybe-unparse (llist)
  (if (symbolp llist)
      llist
      (lambda-list-unparse llist)))

(defmethod multiple-clause-unparse-spec
    ((clause destructuring-regular-clause) spec)
  (maybe-unparse spec))
(defmethod multiple-clause-unparse-spec
    ((clause destructuring-optional-clause) spec)
  (cond ((third spec) (list* (maybe-unparse (first spec)) (rest spec)))
	(t (list (maybe-unparse (first spec)) (second spec)))))
(defmethod multiple-clause-unparse-spec
    ((clause destructuring-key-clause) spec)
  (destructuring-bind ((key var) default -p) spec
    (let ((first (if (and (symbolp var)
			  (string= (symbol-name key) (symbol-name var)))
		     var
		     (list key (maybe-unparse var)))))
      (if -p
	  (list first default -p)
	  (list first default)))))
