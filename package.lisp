;;;; package.lisp
(defpackage #:eslisp-ast
  (:use #:cl #:alexandria #:fiveam #:str)
  (:shadow #:eslisp-ast :test :prefix)
  (:shadow #:str :emptyp))

(defpackage #:eslisp
  (:use #:cl #:alexandria #:eslisp-ast #:str #:fiveam)
  (:shadow #:str :emptyp)
  (:shadow #:eslisp-ast :body :value :prefix :test))
