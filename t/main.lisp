;;;; main.lisp --- self tests for org-doc (plain asserts, zero deps)
(defpackage :org-doc/tests
  (:use :cl)
  (:export #:run))

(in-package :org-doc/tests)

(defmethod asdf:perform ((o asdf:test-op) (s (eql (asdf:find-system :org-doc/tests))))
  (run))

(defparameter *failures* 0)
(defparameter *checks* 0)

(defmacro is (test-form &optional description)
  `(progn
     (incf *checks*)
     (unless ,test-form
       (incf *failures*)
       (format *error-output* "~&FAIL: ~a~%" (or ,description ',test-form)))))

(defun read-file-string (path)
  (uiop:read-file-string path))

(defun run ()
  (setf *failures* 0 *checks* 0)
  ;; Unit: helpers
  (is (equal (org-doc::first-line "alpha
beta") "alpha") "first-line")
  (is (equal (org-doc::file-slug "a.b.c") "a-b-c") "file-slug")
  (is (equal (org-doc::kind-tag :macro) "macro") "kind-tag")

  ;; Integration: generate docs for the fixture into a temp dir
  (let* ((tmp (uiop:temporary-directory))
         (out (uiop:merge-pathnames* "org-doc-test-output/" tmp))
         (fixture-asd (asdf:system-source-directory :org-doc)))
    (asdf:load-asd (uiop:merge-pathnames* "t/org-doc-fixture.asd" fixture-asd))
    (asdf:load-system :org-doc-fixture :verbose nil)

    ;; package discovery must work statically
    (is (member "ORG-DOC.FIXTURE" (org-doc:system-packages :org-doc-fixture)
                :test #'string=)
        "system-packages finds fixture package")

    ;; generate
    (org-doc:generate :org-doc-fixture :output-directory out :quiet t)

    (let ((pkg-doc (read-file-string (uiop:merge-pathnames* "org-doc-fixture.org" out)))
          (index (read-file-string (uiop:merge-pathnames* "index.org" out))))
      (is (search "#+TITLE: API Reference — ORG-DOC.FIXTURE" pkg-doc) "pkg title")
      (is (search ":KIND: fn" pkg-doc) "function kind property")
      (is (search ":KIND: macro" pkg-doc) "macro kind property")
      (is (search ":KIND: class" pkg-doc) "class kind property")
      (is (search ":KIND: condition" pkg-doc) "condition kind property")
      (is (search ":KIND: var" pkg-doc) "variable kind property")
      (is (search "Say hello to NAME." pkg-doc) "first line summary")
      (is (search "#+begin_src lisp" pkg-doc) "org docstring body verbatim")
      (is (search "Points are immutable in spirit." pkg-doc) "multi-line docstring")
      (is (search "Undocumented." pkg-doc) "undocumented marker present for gap")
      (is (search "[[file:org-doc-fixture.org][ORG-DOC.FIXTURE]]" index) "index links package")

      ;; coverage report
      (let ((report (org-doc:coverage :org-doc-fixture)))
        (is (plusp (getf report :symbols)) "coverage counts symbols")
        (is (plusp (length (getf report :undocumented)))
            "coverage finds undocumented"))))

  (format *standard-output* "~&org-doc/tests: ~a checks, ~a failures~%"
          *checks* *failures*)
  (if (plusp *failures*)
      (error "org-doc tests failed: ~a of ~a" *failures* *checks*)
      (values)))
