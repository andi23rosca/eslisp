;;;; package.lisp
(defpackage #:eslisp-ast
  (:use #:cl #:alexandria #:sanity-clause #:str)
  (:shadow #:sanity-clause :load))

(defpackage #:eslisp
  (:use #:cl #:alexandria #:eslisp-ast #:str))
