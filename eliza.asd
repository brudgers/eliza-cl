(load "../utils/norvig-utils")
(load "../utils/norvig-debugger")

(defpackage #:eliza-asd
  (:use :common-lisp
        :asdf
        :norvig-utils
        :norvig-debugger
        :kludgecode))

(in-package :eliza-asd)

(defsystem eliza
    :serial t
    :components ((:file "pat-match")))
