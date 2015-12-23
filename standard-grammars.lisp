(in-package #:sandalphon.lambda-list)

(defun compiler-macro-arguments (form)
  (if (and (eq (car form) 'cl:funcall))
      (cddr form)
      (cdr form)))

(defparameter *ordinary-lambda-list*
  '(regular-clause
    optional-clause
    rest-clause
    key-clause
    aux-clause))

(defparameter *specialized-lambda-list*
  '(specialized-regular-clause
    optional-clause
    rest-clause
    key-clause
    aux-clause))

(defparameter *destructuring-lambda-list*
  '(whole-clause
    (destructuring-regular-clause
     :destructure *destructuring-lambda-list*)
    (destructuring-optional-clause
     :destructure *destructuring-lambda-list*)
    (rest-clause :data-destructure t :keywords (&rest &body))
    (destructuring-key-clause :destructure *destructuring-lambda-list*)
    aux-clause))

(defparameter *macro-lambda-list*
  '((whole-clause :map cdr)
    (destructuring-regular-clause
     :destructure *destructuring-lambda-list*)
    (destructuring-optional-clause
     :destructure *destructuring-lambda-list*)
    (rest-clause :data-destructure t :keywords (&rest &body))
    (destructuring-key-clause :destructure *destructuring-lambda-list*)
    aux-clause
    (environment-clause :anywhere t)))

(defparameter *compiler-macro-lambda-list*
  '((whole-clause :map compiler-macro-arguments)
    (destructuring-regular-clause
     :destructure *destructuring-lambda-list*)
    (destructuring-optional-clause
     :destructure *destructuring-lambda-list*)
    (rest-clause :data-destructure t :keywords (&rest &body))
    (destructuring-key-clause :destructure *destructuring-lambda-list*)
    aux-clause
    (environment-clause :anywhere t)))

(defparameter *defsetf-lambda-list*
  '(regular-clause
    optional-clause
    rest-clause
    key-clause
    environment-clause))

(defparameter *deftype-lambda-list*
  '((whole-clause :map cdr)
    (destructuring-regular-clause
     :destructure *destructuring-deftype-lambda-list*) 
    (destructuring-optional-clause :default-default '*
     :destructure *destructuring-deftype-lambda-list*)
    (rest-clause :data-destructure t :keywords (&rest &body))
    (destructuring-key-clause :default-default '*
     :destructure *destructuring-deftype-lambda-list*)
    aux-clause
    (environment-clause :anywhere t)))

(defparameter *destructuring-deftype-lambda-list*
  '((whole-clause :map cdr)
    (destructuring-regular-clause
     :destructure *destructuring-deftype-lambda-list*)
    (destructuring-optional-clause :default-default '*
     :destructure *destructuring-deftype-lambda-list*)
    (rest-clause :data-destructure t :keywords (&rest &body))
    (destructuring-key-clause :default-default '*
     :destructure *destructuring-deftype-lambda-list*)
    aux-clause))

(defparameter *define-modify-macro-lambda-list*
  '(regular-clause
    optional-clause
    rest-clause))

(defparameter *define-method-combination-lambda-list*
  '(whole-clause
    regular-clause
    optional-clause
    rest-clause
    key-clause
    aux-clause))
