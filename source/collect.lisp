;;;; collect.lisp --- discover packages and extract documentation entries
(in-package :org-doc)

(defstruct (entry (:constructor make-entry))
  package
  name
  kind                    ; keyword: :function :generic-function :macro :class
                          ; :condition :structure :type :constant :special-variable
                          ; :symbol-macro
  lambda-list
  docstring
  slots)                  ; alist of (slot-name . slot-doc) for classes

(defun first-line (string)
  "Return the first non-empty line of STRING, trimmed."
  (when string
    (with-input-from-string (s string)
      (loop for line = (read-line s nil nil)
            thereis (and line (string/= (string-trim " " line) "")
                         (string-trim " " line))))))

(defun package-documentation-string (package)
  (documentation (find-package package) t))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require :sb-introspect)))

(defun safe-lambda-list (symbol)
  "Best-effort lambda list via SB-INTROSPECT when available."
  #+sbcl
  (let ((fn (and (find-package :sb-introspect)
                 (find-symbol "FUNCTION-LAMBDA-LIST" :sb-introspect))))
    (when fn (ignore-errors (funcall fn symbol))))
  #-sbcl nil)

(defun class-slots (symbol)
  "Return a list of (INITARG SLOT-NAME SLOT-DOC) for direct slots of the
   class SYMBOL.  Uses the MOP softly so we never hard-require it."
  (ignore-errors
   (let ((class (find-class symbol nil)))
     (when class
       (let ((direct (and (find-package :sb-mop)
                          (let ((f (find-symbol "CLASS-DIRECT-SLOTS" :sb-mop)))
                            (when f (funcall f class))))))
         (when direct
           (loop for slot in direct
                 collect (list (first (ignore-errors (funcall
                            (find-symbol "SLOT-DEFINITION-INITARGS" :sb-mop) slot)))
                           (funcall (find-symbol "SLOT-DEFINITION-NAME" :sb-mop) slot)
                           (documentation slot t)))))))))

(defun classify (symbol)
  "Classify the external SYMBOL into a doc-kind keyword, or NIL if nothing
   documentable is bound/defined under it."
  (declare (ignorable))
  (cond
    ((macro-function symbol) :macro)
    ((special-operator-p symbol) nil)
    ((fboundp symbol)
     (let ((fn (ignore-errors (fdefinition symbol))))
       (if (and fn (typep fn 'generic-function))
           :generic-function
           :function)))
    ((find-class symbol nil)
     (let ((class (find-class symbol)))
       (cond
         ((subtypep class 'condition) :condition)
         #+sbcl ((typep class 'structure-class) :structure)
         (t :class))))
    ((constantp symbol) :constant)
    ((boundp symbol) :special-variable)
    ((ignore-errors (documentation symbol 'type)) :type)
    ((ignore-errors (documentation symbol 'variable)) :special-variable)
    (t nil)))

(defun docstring-for (symbol kind)
  (ecase kind
    ((:function :generic-function :macro)
     (documentation symbol 'function))
    ((:class :condition :structure :type)
     (or (documentation symbol 'type)
         (ignore-errors (documentation (find-class symbol) t))))
    ((:constant :special-variable)
     (documentation symbol 'variable))))

;;;; Static source fallback
;;;
;;; Some implementations drop defun docstrings for functions whose lambda
;;; lists contain function constants (e.g. #:*default-arg* #'function).
;;; Since the source is the documentation source of truth, we can always
;;; read docstrings straight out of the files.

(defvar *source-doc-table* (make-hash-table :test 'equal)
  "name (string, lowercased) -> docstring, harvested from source files.")

(defun string-docstring (string)
  (when (and string (plusp (length string))) string))

(defun read-doc-forms (file)
  "Read top-level forms of FILE honoring in-package, returning them plus
   any errors ignored.  Returns a list of forms (best effort)."
  (ignore-errors
   (with-open-file (in file)
     (let* ((*read-eval* nil)
            (forms '()))
       (handler-case
           (loop for form = (read in nil nil)
                 while form
                 do (push form forms)
                    (when (and (consp form)
                               (eq (first form) 'in-package)
                               (symbolp (second form)))
                      (let ((pkg (find-package (second form))))
                        (when pkg
                          (setf *package* pkg)))))
         (end-of-file ()))
       (nreverse forms)))))

(defun quoted-symbol (x)
  "Return the symbol of X, unwrapping (QUOTE SYM) when present."
  (cond ((and (consp x) (eq (first x) 'quote) (symbolp (second x)))
         (second x))
        ((symbolp x) x)
        (t nil)))

(defun harvest-static-docs (file)
  "Collect docstrings for definitions in FILE into *STATIC-DOCS*."
  (let ((forms (read-doc-forms file)))
    (dolist (form forms)
      (when (consp form)
        (let ((op (first form)))
          (cond
            ((and (member op '(defun defmacro))
                  (symbolp (second form)))
             (let ((doc (third form)))
               (when (and (stringp doc)
                          (not (keywordp doc)))
                 (setf (gethash (symbol-name (second form)) *source-doc-table*)
                       doc))))
            ((and (eq op 'defgeneric) (symbolp (second form)))
             (dolist (opt (cdddr form))
               (when (and (consp opt) (eq (first opt) :documentation)
                          (stringp (second opt)))
                 (setf (gethash (symbol-name (second form)) *source-doc-table*)
                       (second opt)))))
            ((and (member op '(defvar defparameter defconstant))
                  (symbolp (second form)))
             (let ((doc (or (fourth form) (third form))))
               (when (stringp doc)
                 (setf (gethash (symbol-name (second form)) *source-doc-table*)
                       doc))))
            ((and (member op '(defclass define-condition))
                  (symbolp (second form)))
             (dolist (opt (cdddr form))
               (when (and (consp opt) (eq (first opt) :documentation)
                          (stringp (second opt)))
                 (setf (gethash (symbol-name (second form)) *source-doc-table*)
                       (second opt)))))
            ((eq op 'defstruct)
             ;; docstring may follow the name or the (name options...) list
             (let* ((rest (cdr form))
                    (first (first rest))
                    (name (cond ((symbolp first) first)
                                ((consp first) (first first))))
                    (doc (cond
                           ((and (consp first) (stringp (second rest)))
                            (second rest))
                           ((and (symbolp first) (stringp (second rest)))
                            (second rest))
                           ((stringp first) first))))
               (when (and name doc)
                 (setf (gethash (symbol-name name) *source-doc-table*)
                       doc))))
            ((and (eq op 'setf) (consp (second form))
                  (eq (first (second form)) 'documentation))
             (let* ((target (quoted-symbol (second (second form))))
                    (doc-type (quoted-symbol (third (second form))))
                    (doc (third form)))
               (when (and target (stringp doc)
                          (member doc-type '(function variable type)))
                 (setf (gethash (symbol-name target) *source-doc-table*)
                       doc))))))))))

(defun harvest-system-docs (system-name)
  "Harvest docstrings from every source file of SYSTEM-NAME into
   *STATIC-DOCS*.  Safe to call repeatedly; results accumulate."
  (dolist (file (system-source-files system-name))
    (harvest-static-docs file))
  (hash-table-count *source-doc-table*))

(defun static-docstring-for (symbol)
  (gethash (symbol-name symbol) *source-doc-table*))

(defun collect-package (package-designator &key (external-only t))
  "Extract documentation entries for every documentable symbol exported
   from PACKAGE-DESIGNATOR."
  (let ((package (if (packagep package-designator)
                     package-designator
                     (find-package package-designator)))
        (entries))
    (unless package
      (error "Package not found: ~a" package-designator))
    (labels ((handle (symbol)
               (let ((kind (classify symbol)))
                 (when kind
                   (let ((doc (docstring-for symbol kind)))
                     (push (make-entry
                            :package (package-name package)
                            :name (symbol-name symbol)
                            :kind kind
                            :lambda-list
                            (when (member kind '(:function :generic-function :macro))
                              (safe-lambda-list symbol))
                            :docstring doc
                            :slots (when (member kind '(:class :condition :structure))
                                     (class-slots symbol)))
                           entries))))))
      (if external-only
          (do-external-symbols (s package) (handle s))
          (do-symbols (s package)
            (when (eq (symbol-package s) package)
              (handle s)))))
    (sort entries #'string< :key #'entry-name)))

(defun system-source-files (system)
  "Return the list of Common Lisp source pathnames of ASDF SYSTEM by
   walking its component tree."
  (let ((sys (if (typep system 'asdf:system) system (asdf:find-system system))))
    (labels ((walk (component)
               (typecase component
                 (asdf:cl-source-file
                  (list (asdf:component-pathname component)))
                 (asdf:module
                  (mapcan #'walk (asdf:component-children component)))
                 (t nil))))
      (walk sys))))

(defun read-package-names (file)
  "Static scan: read top-level forms of FILE and return package names
   defined by defpackage / uiop:define-package / cl:defpackage."
  (ignore-errors
   (with-open-file (in file)
     (let* ((*package* (find-package :cl))
            (*read-eval* nil)
            (names '()))
       (handler-case
           (loop for form = (read in nil nil)
                 while form
                 do (when (and (consp form)
                               (member (car form) '(defpackage uiop:define-package)))
                      (let ((name (cadr form)))
                        (when (and name (or (symbolp name) (stringp name)))
                          (pushnew (string name) names :test #'string=)))))
         (end-of-file ()))
       (nreverse names)))))

(defun system-packages (system-designator)
  "Return the names of all packages defined by the source files of the
   ASDF system SYSTEM-DESIGNATOR.  Works without loading the system.
   A trailing :external or :all keyword argument is not needed here;
   use COLLECT-PACKAGE for extraction policy."
  (let ((system (asdf:find-system system-designator)))
    (remove-duplicates
     (append
      (loop for file in (system-source-files system)
            append (read-package-names file))
      ;; Also pick up packages already in the image whose name is mentioned
      ;; by the system name itself (covers generated packages).
      nil)
     :test #'string=)))

(defun only-live-packages (names)
  "Filter NAMES down to packages that actually exist in the image."
  (remove-if-not #'find-package names))
