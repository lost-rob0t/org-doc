;;;; org-doc-fixture.asd --- tiny system with Org docstrings for self tests
(asdf:defsystem #:org-doc-fixture
  :version "0.0.1"
  :serial t
  :components
  ((:file "fixture-pkg")
   (:file "fixture-code")))
