;;;; package.lisp
(defpackage #:eslisp-ast
  (:use #:cl #:alexandria #:fiveam #:str)
  (:shadow #:eslisp-ast :test :prefix))

(defpackage #:eslisp
  (:use #:cl #:alexandria #:eslisp-ast #:str))
