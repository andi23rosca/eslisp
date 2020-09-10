;;;; eslisp.lisp

(in-package #:eslisp)


(defun eslisp->ast (program)
  "Turns eslisp syntax into javascript AST"
  (parse-eslisp program))

(defun eslisp (program)
  (let* ((expanded (expand-all program))
         (parsed (eslisp->ast expanded))
         (javascript (ast->javascript)))))
