(cl:in-package :eslisp)

(defun is-hardcoded-computed (expr)
  (and (listp expr) (equal (car expr) :expr)))
(defun is-computed-subtype (expr)
  (and
    (subtypep (type-of expr) 'es-expression)
    (not (subtypep (type-of expr) 'es-identifier))
    (not (subtypep (type-of expr) 'es-call-expression))))

(defun is-chain-computed (expr)
  (or
   (is-hardcoded-computed expr)
   (is-computed-subtype expr)))

(defun build-chain (expr)
  (let ((rev (reverse expr))
        (res nil))
    (if (< (length expr) 2)
        (setf res (car expr))
        (let ((expanded
                (mapcar (lambda (e)
                          (cond ((is-computed-subtype e) (list :expr e))
                                ((is-hardcoded-computed e) e)
                                (t (list :dot e))))
                        rev) ))
          (setf res (cadr (car expanded)))
          (loop
            :for parent :in (cdr expanded)
            :for child = (car expanded) then parent
            :do (setf res
                      (make-instance 'es-member-expression
                                     :object (cadr parent)
                                     :property res
                                     :computed (is-hardcoded-computed child))))))
    res))


(defun parse-chaining (expr)
  (let* ((args (cdr expr))
         (parsed
           (mapcar (lambda (a)
                     (if (and (listp a)
                              (equal (car a) :expr))
                         (list :expr (parse-eslisp (cadr a)))
                         (parse-eslisp a)))
                   args)))
    (build-chain parsed)))
