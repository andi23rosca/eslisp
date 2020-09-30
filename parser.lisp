(in-package :eslisp)


(defun stringify (expr)
  (format nil "~(~a~)" expr))

(defun symbol-test (sym test)
  (if (listp test)
      (find sym test :test #'equal)
      (equal test sym)))

(defun parse-declarator (dec)
  (make-instance 'es-variable-declarator
                 :id (parse-eslisp (car dec))
                 :init (when (cadr dec)
                         (parse-eslisp (cadr dec)))))
(defun parse-declaration (expr)
  (let ((kind (stringify (car expr)))
        (vars (cdr expr)))
    (make-instance 'es-container
                   :items (mapcar
                           (lambda (dec)
                             (make-instance 'es-variable-declaration
                                            :kind kind
                                            :declarations (list (parse-declarator dec))))
     vars))))


(defun parse-eslisp (expr)
  (print expr)
  (if (listp expr)
      (let ((result nil)
            (sym (car expr)))
        (setf result
              (switch (sym :test #'symbol-test)
                ('(const let) (parse-declaration expr))))
        (when (not result)
          (setf result :todo))
        result)
      (make-instance 'es-literal :value (stringify expr))))
