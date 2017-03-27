(asdf:load-system :lisp-unit)

(defpackage :eliza-test
  (:use :common-lisp
        :eliza
        :lisp-unit))

(in-package :eliza-test)

(define-test test-one
    (assert-equal 5 5))
