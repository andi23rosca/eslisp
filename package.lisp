;;;; package.lisp
(defpackage #:eslisp-ast
  (:use #:cl #:alexandria #:sanity-clause #:str)
  (:shadow :load :emptyp))

(defpackage #:eslisp
  (:use #:cl #:alexandria #:eslisp-ast #:str) )
