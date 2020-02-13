;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :s-graphviz.system)
    (defpackage :s-graphviz.system
      (:use :cl :asdf))))

(in-package :s-graphviz.system)

(asdf:defsystem s-graphviz
  :author "Xu Jingtao <jingtaozf@gmail.com>"
  :version "2.0"
  :licence "MIT"
  :serial t
  :description "a s-expression presentation of GraphViz DOT language"
  :defsystem-depends-on ("literate-lisp")
  :depends-on ((:version :iterate "1.5"))
  :components ((:module :demo :pathname "./"
                        :components ((:org "s-graphviz")))))
