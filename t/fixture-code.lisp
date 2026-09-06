;;;; fixture-code.lisp --- Org docstrings covering every kind
(in-package :org-doc.fixture)

(defparameter *flavor* "vanilla"
  "Current ice cream *flavor*.

Examples:
- vanilla
- chocolate")

(defconstant pi-squared 9.8696044
  "Pi, squared, for dubious numerology.")

(defclass point ()
  ((x :initarg :x :documentation "Horizontal coordinate.")
   (y :initarg :y :documentation "Vertical coordinate."))
  (:documentation "A *2D point*.

* Note
Points are immutable in spirit."))

(define-condition point-error (error)
  ((code :initarg :code))
  (:documentation "Signalled when a =point= misbehaves."))

(defgeneric describe-thing (thing)
  (:documentation "Describe THING in a human way."))

(defmethod describe-thing ((thing point))
  "Describe a point."
  (format nil "point at ~s" (list (slot-value thing 'x))))

(defun greet (&optional (name "world"))
  "Say hello to NAME.

#+begin_src lisp
(greet \"star\") => \"hello, star\"
#+end_src"
  (format nil "hello, ~a" name))

(defmacro with-greeting ((name) &body body)
  "Bind =it= to a greeting for NAME while evaluating BODY."
  `(let ((it (greet ,name))) ,@body))

(defun undocumented-fn (x)
  x)
