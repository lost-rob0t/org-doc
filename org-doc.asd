;;;; org-doc.asd --- Org-mode API documentation from Common Lisp docstrings
(asdf:defsystem #:org-doc
  :version "0.1.0"
  :description "Write Org-mode docstrings in source; render them as Org API docs."
  :author "nsaspy@airmail.cc"
  :license "GPL-3.0-or-later"
  :depends-on ()
  :in-order-to ((test-op (test-op #:org-doc/tests)))
  :components
  ((:module "source"
    :serial t
    :components
    ((:file "package")
     (:file "collect")
     (:file "render")
     (:file "generate")
     (:file "cli")))))

(asdf:defsystem #:org-doc/tests
  :description "Self tests for org-doc."
  :depends-on (#:org-doc)
  :components
  ((:module "t"
    :serial t
    :components
    ((:file "main")))))
