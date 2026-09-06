;;;; package.lisp --- org-doc packages
(defpackage :org-doc
  (:use :cl)
  (:export
   ;; entry points
   #:generate
   #:coverage
   #:system-packages
   #:system-source-files
   ;; entry records
   #:entry
   #:entry-package
   #:entry-name
   #:entry-kind
   #:entry-lambda-list
   #:entry-docstring
   #:entry-slots
   ;; rendering knobs
   #:*undocumented-marker*))

(defpackage :org-doc/cli
  (:use :cl)
  (:export #:main))
