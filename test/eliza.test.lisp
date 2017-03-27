(asdf:load-system :lisp-unit)

(defpackage :eliza-test
  (:use :common-lisp
        :asdf
        :eliza
        :lisp-unit))

(in-package :eliza-test)

(defsystem eliza-test
    :serial t
    :components ((:file "eliza.test")))

(define-test test-one
    (assert-equal 5 5))
