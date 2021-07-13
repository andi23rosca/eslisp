(cl:in-package :eslisp)

(defun parse-fncall (expr)
  (let ((parsed (mapcar #'parse-eslisp expr)))
    (make-instance 'es-call-expression
                   :callee (car parsed)
                   :arguments (cdr parsed))))
