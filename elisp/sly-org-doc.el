;;; sly-org-doc.el --- Live Org docstrings for SLY and SLIME -*- lexical-binding: t; -*-

;; Author: nsaspy <nsaspy@airmail.cc>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, docs, org, sly, slime, eldoc
;; URL: https://git.starintel.actor/nsaspy/org-doc

;;; Commentary:

;; Org docstrings, live, as you type.
;;
;; Pair this package with the `org-doc' Common Lisp library, which renders
;; the Org markup you write inside Common Lisp docstrings into an Org API
;; reference under doc/api/.  This package surfaces those same docstrings
;; inside Emacs:
;;
;;  - `sly-org-doc-eldoc-mode' shows the first line of the docstring in the
;;    echo area next to the arglist, via eldoc, while SLY or SLIME is
;;    connected.
;;
;;  - `sly-org-doc-describe-symbol' (C-c C-d C-o) opens the full docstring
;;    in a fontified org-mode buffer, so headings, code blocks and links
;;    in the docstring actually render.
;;
;;  - `sly-org-doc-goto-source-doc' jumps to the docstring in the source.
;;
;; Usage:
;;
;;   (require 'sly-org-doc)
;;   (add-hook 'sly-mode-hook #'sly-org-doc-eldoc-mode)
;;   (add-hook 'sly-mrepl-mode-hook #'sly-org-doc-eldoc-mode)
;;   (add-hook 'slime-mode-hook #'sly-org-doc-eldoc-mode)
;;
;; Docstring convention (Org markup in Common Lisp docstrings):
;;
;;   (defun greet (&optional (name "world"))
;;     "Say hello to NAME.
;;
;;   #+begin_src lisp
;;   (greet \"star\") => \"hello, star\"
;;   #+end_src"
;;     ...)

;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'org)

(defgroup sly-org-doc nil
  "Org docstrings for Common Lisp via SLY/SLIME."
  :group 'lisp
  :prefix "sly-org-doc-")

(defcustom sly-org-doc-eldoc-limit 1
  "How many lines of the docstring to show in the echo area."
  :type 'natnum)

(defcustom sly-org-doc-eldoc-width 72
  "Maximum width of the docstring summary in the echo area."
  :type 'natnum)

(defface sly-org-doc-eldoc-doc-face
  '((t :inherit font-lock-doc-face :slant italic))
  "Face for the org docstring summary in the echo area.")

(defvar-local sly-org-doc--cache (make-hash-table :test 'equal)
  "Per-buffer cache of symbol documentation lookups.")

(defvar sly-mode-map)
(defvar slime-mode-map)

(defun sly-org-doc--connected-p ()
  "Return non-nil if a SLY or SLIME connection is live."
  (or (and (fboundp 'sly-connected-p) (sly-connected-p))
      (and (fboundp 'slime-connected-p) (slime-connected-p))))

(defun sly-org-doc--eval (form)
  "Evaluate FORM in the Lisp image, supporting SLY and SLIME."
  (cond
   ((and (fboundp 'sly-eval) (bound-and-true-p sly-mode))
    (sly-eval form))
   ((and (fboundp 'slime-eval) (bound-and-true-p slime-mode))
    (slime-eval form))
   ((and (fboundp 'sly-eval) (sly-connected-p))
    (sly-eval form))
   ((and (fboundp 'slime-eval) (slime-connected-p))
    (slime-eval form))
   (t nil)))

(defun sly-org-doc--fetch (symbol)
  "Fetch (FUNCTION-DOC VARIABLE-DOC TYPE-DOC) for SYMBOL from the image.
Cached per buffer.  Returns a list of strings or nil."
  (let* ((key (format "%s" symbol))
         (hit (gethash key sly-org-doc--cache)))
    (or (and (consp hit) hit)
        (when (sly-org-doc--connected-p)
          (let* ((form `(cl:values
                         (cl:ignore-errors (cl:documentation ',symbol 'function))
                         (cl:ignore-errors (cl:documentation ',symbol 'variable))
                         (cl:ignore-errors (cl:documentation ',symbol 'type))))
                 (res (ignore-errors (sly-org-doc--eval form))))
            (when res
              (let ((val (mapcar (lambda (d) (if (stringp d) d "")) res)))
                ;; Do not cache the "nothing found" case; connection
                ;; may still be loading the system.
                (puthash key val sly-org-doc--cache)
                val)))))))

(defun sly-org-doc--clear-cache ()
  "Forget cached documentation lookups (run after eval/compile)."
  (clrhash sly-org-doc--cache))

(defun sly-org-doc--org-plain (doc limit)
  "Return the first LIMIT lines of org DOC with markup stripped."
  (when (stringp doc)
    (let* ((lines (cl-remove-if
                   (lambda (l) (string-prefix-p "#+" l))
                   (split-string doc "\n")))
           (picked (cl-subseq lines 0 (min limit (length lines))))
           (text (mapconcat
                  (lambda (l)
                    (replace-regexp-in-string
                     "[=~*_]" "" (string-trim l)))
                  picked " "))
           (text (replace-regexp-in-string "[ \t\n]+" " " text)))
      (when (> (length text) sly-org-doc-eldoc-width)
        (setq text (substring text 0 sly-org-doc-eldoc-width)))
      text)))

(defun sly-org-doc--eldoc-function (callback &rest _ignored)
  "Eldoc function showing the org docstring summary at point.
For `eldoc-documentation-functions'."
  (let ((sym (thing-at-point 'symbol t)))
    (when (and sym (sly-org-doc--connected-p))
      (let* ((docs (sly-org-doc--fetch (intern sym)))
             (doc (car (cl-remove-if #'string-empty-p docs))))
        (when doc
          (let ((text (sly-org-doc--org-plain doc sly-org-doc-eldoc-limit)))
            (when (and text (not (string-empty-p text)))
              (funcall callback
                       (propertize text 'face 'sly-org-doc-eldoc-doc-face)
                       :thing (intern sym)
                       :face 'font-lock-function-name-face))))))))

(defun sly-org-doc--kind-of (symbol)
  "Return a human kind string for SYMBOL, best effort."
  (let* ((docs (sly-org-doc--fetch symbol))
         (fdoc (nth 0 docs)) (vdoc (nth 1 docs)) (tdoc (nth 2 docs)))
    (cond ((and fdoc (fboundp symbol))
           (if (and (fboundp 'sly-eval))
               (cond ((ignore-errors
                        (sly-org-doc--eval
                         `(cl:typep (cl:fdefinition ',symbol) 'cl:generic-function)))
                      "generic-function")
                     ((ignore-errors
                        (sly-org-doc--eval
                         `(cl:typep (cl:symbol-function ',symbol) 'cl:standard-generic-function)))
                      "generic-function")
                     (t "function")))
           "function")
          (tdoc "type/class")
          (vdoc "variable")
          (t "symbol"))))

(defun sly-org-doc--package-of (symbol)
  "Return the home package of SYMBOL as a string, or nil."
  (ignore-errors
    (sly-org-doc--eval
     `(cl:package-name (cl:symbol-package ',symbol)))))

;;;###autoload
(defun sly-org-doc-describe-symbol (symbol)
  "Describe SYMBOL showing its Org docstring fontified as Org.
Interactively, uses the symbol at point."
  (interactive (list (intern (thing-at-point 'symbol t))))
  (unless symbol (user-error "No symbol at point"))
  (let* ((docs (sly-org-doc--fetch symbol))
         (doc (or (cl-find-if (lambda (d) (and (stringp d) (not (string-empty-p d)))) docs)
                  "Undocumented."))
         (kind (sly-org-doc--kind-of symbol))
         (pkg (sly-org-doc--package-of symbol))
         (buf (get-buffer-create (format "*Org Doc: %s*" symbol))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: %s\n" symbol))
        (insert (format ":PROPERTIES:\n:KIND: %s\n:PACKAGE: %s\n:END:\n\n"
                        kind (or pkg "?")))
        (insert (format "* Documentation\n\n%s\n" doc))
        (org-mode)
        (goto-char (point-min))
        (setq-local header-line-format
                    (format " %s %s%s" kind symbol
                            (if pkg (format " (%s)" pkg) "")))
        (setq buffer-read-only t)))
    (pop-to-buffer buf)))

(defun sly-org-doc-goto-source-doc (symbol)
  "Open the source location of SYMBOL's docstring via SLY/SLIME."
  (interactive (list (intern (thing-at-point 'symbol t))))
  (cond
   ((fboundp 'sly-edit-definition) (sly-edit-definition (symbol-name symbol)))
   ((fboundp 'slime-edit-definition) (slime-edit-definition (symbol-name symbol)))
   (t (user-error "No SLY or SLIME available"))))

;;; Minor mode

(defvar sly-org-doc-eldoc-mode-map (make-sparse-keymap))

(defun sly-org-doc--setup-eldoc ()
  "Add our eldoc function and compose it with SLY/SLIME autodoc."
  (add-hook 'eldoc-documentation-functions #'sly-org-doc--eldoc-function nil t)
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose))

(defun sly-org-doc--teardown-eldoc ()
  (remove-hook 'eldoc-documentation-functions #'sly-org-doc--eldoc-function t))

(defun sly-org-doc--after-change (&rest _)
  "Forget cached doc lookups after the buffer changed."
  (sly-org-doc--clear-cache))

;;;###autoload
(define-minor-mode sly-org-doc-eldoc-mode
  "Show Org docstring summaries in the echo area as you type.
Composes with \\=`sly-autodoc' and \\=`slime-eldoc' arglists."
  :lighter " orgdoc"
  :keymap sly-org-doc-eldoc-mode-map
  (cond
   (sly-org-doc-eldoc-mode
    (sly-org-doc--setup-eldoc)
    (add-hook 'after-change-functions #'sly-org-doc--after-change nil t)
    (eldoc-mode 1))
   (t
    (sly-org-doc--teardown-eldoc)
    (remove-hook 'after-change-functions #'sly-org-doc--after-change t))))

;;;###autoload
(define-globalized-minor-mode global-sly-org-doc-eldoc-mode
  sly-org-doc-eldoc-mode
  (lambda ()
    (when (derived-mode-p 'lisp-mode 'sly-mode 'sly-mrepl-mode 'slime-mode)
      (sly-org-doc-eldoc-mode 1))))

(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c C-d C-o") #'sly-org-doc-describe-symbol)
  (define-key sly-mode-map (kbd "C-c C-d M-o") #'sly-org-doc-goto-source-doc))

(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c C-d C-o") #'sly-org-doc-describe-symbol)
  (define-key slime-mode-map (kbd "C-c C-d M-o") #'sly-org-doc-goto-source-doc))

(provide 'sly-org-doc)
;;; sly-org-doc.el ends here
