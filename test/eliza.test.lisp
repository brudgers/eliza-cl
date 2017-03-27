(asdf:load-system :lisp-unit)

(in-package :eliza)

(define-test test-one
    (assert-equal 5 5))
