(in-package :eslisp)


(defun stringify (expr)
  (format nil "~(~a~)" expr))

(defun symbol-test (sym test)
  (if (listp test)
      (find sym test :test #'equal)
      (equal test sym)))



(defun parse-eslisp (expr)
  ;; (print expr)
  (if (listp expr)
      (let ((result nil)
            (sym (car expr)))
        (setf result
              ;; When expression is a list we parse the syntax
              (switch (sym :test #'symbol-test)
                ('(const let) (parse-declaration expr))
                ('this (make-instance 'es-this-expression))))
        (when (not result)
          (setf result :todo))
        result)
      ;; Case when the expression is not a list
      ;; It can either be a literal or an identifier
      (if (is-jslit expr)
          (parse-literal expr)
          (parse-symbol expr))))
