;;;; cli.lisp --- flag-based CLI, position independent of sbcl --script argv quirks
(in-package :org-doc/cli)

(defun argv-all ()
  #+sbcl sb-ext:*posix-argv*
  #-sbcl (uiop:command-line-arguments))

(defun get-option (argv flag &optional default)
  "Return the value following FLAG in ARGV, or DEFAULT."
  (let ((pos (position flag argv :test #'string=)))
    (when pos
      (let ((rest (subseq argv (1+ pos))))
        (if rest (first rest) default)))))

(defun has-flag (argv flag)
  (member flag argv :test #'string=))

(defun usage (stream)
  (format stream "~&org-doc — render Org API docs from Org docstrings.~%~
Usage: org-doc --system SYSTEM [--out DIR] [--packages P1,P2]~
 [--internal] [--no-load] [--index NAME] [--quiet]~%"))

(defun main ()
  (let* ((argv (argv-all))
         (system (get-option argv "--system"))
         (out (or (get-option argv "--out") "doc/api/"))
         (pkgs (get-option argv "--packages"))
         (index (or (get-option argv "--index") "index"))
         (packages (when pkgs (uiop:split-string pkgs :separator ",")))
         (quiet (has-flag argv "--quiet")))
    (cond
      ((or (has-flag argv "--help") (has-flag argv "-h") (null system))
       (usage *error-output*)
       (uiop:quit (if system 0 2)))
      (t
       (multiple-value-bind (ok error-condition)
           (ignore-errors
             (org-doc:generate system
                               :output-directory out
                               :packages packages
                               :external-only (not (has-flag argv "--internal"))
                               :load-system (not (has-flag argv "--no-load"))
                               :index-name index
                               :quiet quiet))
         (if ok
             (uiop:quit 0)
             (progn
               (format *error-output* "~&org-doc: failed: ~a~%" error-condition)
               (uiop:quit 1))))))))

(defun main-quit ()
  (main))
