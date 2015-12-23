(in-package #:sandalphon.lambda-list)

(defclass lambda-list ()
  ((clauses :initarg :clauses :accessor lambda-list-clauses)
   (safety :initarg :safe :accessor lambda-list-safe
	   :initform t)))

(defgeneric lambda-list-keywords (lambda-list)
  (:method ((lambda-list lambda-list))
    (loop for clause in (lambda-list-clauses lambda-list)
       appending (clause-keywords clause))))

(defclass clause ()
  ((keywords :initarg :keywords :accessor clause-keywords)
   (safety :initarg :safe :accessor clause-safe :initform t)))

(defclass singleton-clause (clause)
  ((spec :initarg :spec :accessor clause-spec
	 :initform nil))
  (:documentation
   "A clause that only contains one parameter, a symbol."))

(defclass multiple-clause (clause)
  ((specs :initarg :specs :accessor multiple-clause-specs
	  :initform nil))
  (:documentation
   "A clause that contains multiple specifiers, which are treated identically (by MULTIPLE-CLAUSE-BINDS), and which are bound left-to-right."))

(defclass defaulting-clause (clause)
  ((default-default :initarg :default-default
     :initform nil
     :accessor clause-default-default)))

(defclass destructuring-clause (clause)
  ((grammar-name :initarg :destructure
		 :accessor destructuring-clause-grammar)))

(defclass regular-clause (multiple-clause)
  ((keywords :initform '(nil))))
(defclass optional-clause (multiple-clause defaulting-clause)
  ((keywords :initform '(&optional))))
(defclass rest-clause (singleton-clause)
  ((keywords :initform '(&rest))))
(defclass key-clause (multiple-clause defaulting-clause)
  ((keywords :initform '(&key &allow-other-keys))
   (aok-p :initarg :aok-p :accessor key-clause-aok-p :initform nil)))
(defclass aux-clause (multiple-clause defaulting-clause)
  ((keywords :initform '(&aux))))

(defclass specialized-regular-clause (multiple-clause)
  ((keywords :initform '(nil))))

(defclass whole-clause (singleton-clause)
  ((keywords :initform '(&whole))
   (map :initarg :map :accessor whole-map :initform 'identity)))
(defclass environment-clause (singleton-clause)
  ((keywords :initform '(&environment))))

(defclass destructuring-regular-clause
    (multiple-clause destructuring-clause)
  ((keywords :initform '(nil))))
(defclass destructuring-optional-clause
    (multiple-clause defaulting-clause destructuring-clause)
  ((keywords :initform '(&optional))))
(defclass destructuring-key-clause
    (multiple-clause defaulting-clause destructuring-clause)
  ((keywords :initform '(&key &allow-other-keys))
   (aok-p :initarg :aok-p :accessor key-clause-aok-p)))
