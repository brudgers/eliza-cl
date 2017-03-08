(in-package :eliza-asd)

(defconstant fail nil "indicates pat-match failure.")

(defconstant no-bindings '(t . t)
  "Indicates a pattern match success, with no variables.")

(defun get-bindings (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-bindings var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val) bindings))

(defun variable-p (x)
  "Is x a variable where a variable is a symbol beginning with '?'"
  (and (symbolp x)
       (equal (char (symbol-name x) 0)
              #\?)))

(defun match-variable (var input bindings)
  "Does var match input. Uses bindings. Returns bindings with or without an update depending on match."
  (let ((binding (get-bindings var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))
