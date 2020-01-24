;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :dot.system)
    (defpackage :dot.system
      (:use :cl :asdf))))

(in-package :dot.system)

(asdf:defsystem dot
  :author "Xu Jingtao <jingtaozf@gmail.com>"
  :version "2.0"
  :licence "MIT"
  :serial t
  :description "a literate version of dot"
  :defsystem-depends-on ("literate-lisp")
  :depends-on ((:version :iterate "1.5"))
  :components ((:module :demo :pathname "./"
                        :components ((:org "dot")))))
