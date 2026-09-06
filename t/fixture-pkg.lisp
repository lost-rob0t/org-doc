;;;; fixture-pkg.lisp
(uiop:define-package #:org-doc.fixture
  (:use :cl)
  (:documentation "Fixture package for org-doc self tests.")
  (:export
   #:greet
   #:make-point
   #:point
   #:point-error
   #:*flavor*
   #:pi-squared
   #:with-greeting
   #:describe-thing
   #:undocumented-fn))
