(in-package :eliza)

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

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in context of the bindings."
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern)
                    (rest input)
                    (pat-match (first pattern)
                               (first input)
                               bindings)))
        (t fail)))


(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment patter ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume pat starts with a constant
        ;; In other words, a pattern can't have 2 consectutive vars
        (let ((pos (position (first pat)
                             input
                             :start start
                             :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match (subseq input pos)
                                   bindings)))
                ;; If this match failed try another longer one
                ;; If it worked, check that the variables match
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ 1 pos))
                    (match-variable var
                                    (subseq input 0 pos)
                                    b2))))))))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))
