(load "../utils/norvig-utils")
(load "../utils/norvig-debugger")

(defpackage #:eliza
  (:use :common-lisp
        :asdf
        :norvig-utils
        :norvig-debugger
        :kludgecode))

(in-package :eliza)

(defsystem eliza
    :serial t
    :components ((:file "eliza")))
