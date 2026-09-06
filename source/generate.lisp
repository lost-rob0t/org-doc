;;;; generate.lisp --- top-level generation entry points
(in-package :org-doc)

(defun package-entries-for-system (system-name &key packages (external-only t))
  "Collect entries for PACKAGES (defaults to all packages discovered in
   SYSTEM-NAME's sources that exist in the image).  Returns an alist of
   (PACKAGE-NAME ENTRIES)."
  (let* ((names (or packages (only-live-packages (system-packages system-name))))
         (alist))
    (dolist (name names)
      (let ((pkg (find-package name)))
        (when pkg
          (let ((entries (collect-package pkg :external-only external-only)))
            (when entries
              (push (cons (package-name pkg) entries) alist))))))
    (sort alist #'string< :key #'car)))

(defun coverage (system-name &key packages)
  "Return a coverage report plist for SYSTEM-NAME:
   (:packages N :symbols N :documented N :undocumented (SYM...))."
  (let* ((alist (package-entries-for-system system-name :packages packages))
         (all (apply #'append (mapcar #'cdr alist)))
         (undocumented (remove-if #'entry-docstring all)))
    (list :packages (length alist)
          :symbols (length all)
          :documented (- (length all) (length undocumented))
          :undocumented undocumented)))

(defun print-coverage (system-name &key packages (stream *standard-output*))
  "Print a per-package documentation coverage table for SYSTEM-NAME."
  (let* ((alist (package-entries-for-system system-name :packages packages))
         (undocumented (coverage system-name :packages packages)))
    (format stream "~&Package~40tDocumented/Total~%")
    (format stream "----------------------------------------------~%")
    (dolist (pair alist)
      (let* ((entries (cdr pair))
             (have (count-if #'entry-docstring entries)))
        (format stream "~a~40t~d/~d~%" (car pair) have (length entries))))
    (format stream "----------------------------------------------~%")
    (format stream "Total undocumented: ~a~%"
            (length (getf undocumented :undocumented)))
    (dolist (e (getf undocumented :undocumented))
      (format stream "  ~a::~a (~a)~%"
              (entry-package e) (entry-name e) (entry-kind e)))
    (values)))

(defun ensure-directory (path)
  (let ((dir (uiop:ensure-directory-pathname path)))
    (ensure-directories-exist dir)
    dir))

(defun generate (system-name
                 &key
                   (output-directory #p"doc/api/")
                   packages
                   (external-only t)
                   (load-system t)
                   (index-name "index")
                   (quiet nil))
  "Generate Org API documentation for ASDF system SYSTEM-NAME.

   - LOAD-SYSTEM: load the system first (docstrings live in the image).
   - PACKAGES: restrict to these package names; default is every package
     defined by the system's source files.
   - Writes one .org per package plus INDEX-NAME.org into OUTPUT-DIRECTORY.
   Returns the output directory."
  (when load-system
    (asdf:load-system system-name :verbose nil))
  (let* ((alist (package-entries-for-system system-name
                                            :packages packages
                                            :external-only external-only))
         (outdir (ensure-directory output-directory)))
    (dolist (pair alist)
      (destructuring-bind (pkg . entries) pair
        (let ((file (make-pathname :defaults outdir
                                   :name (file-slug pkg) :type "org")))
          (with-open-file (out file :direction :output
                                    :if-exists :supersede)
            (write-string
             (render-package entries pkg
                             :system-name (string system-name)
                             :doc (package-documentation-string pkg))
             out))
          (unless quiet
            (format *standard-output* "~&org-doc: wrote ~a (~a symbols)~%"
                    file (length entries))))))
    (with-open-file (out (make-pathname :defaults outdir
                                        :name index-name :type "org")
                         :direction :output :if-exists :supersede)
      (write-string (render-index alist system-name) out))
    (unless quiet
      (print-coverage system-name :packages packages))
    outdir))
