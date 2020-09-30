;;;; eslisp.lisp

(in-package #:eslisp)


(defun eslisp->ast (program)
  "Turns eslisp syntax into javascript AST"
  (mapcar #'parse-eslisp program))
(defun ast->javascript (program)
  (mapcar #'es->js program))

(defun eslisp (program)
  (let* ((expanded (expand-all program))
         (parsed (eslisp->ast expanded))
         (javascript (ast->javascript parsed)))))
