(cl:in-package :eslisp)

(defun is-jsnum (sym)
  (numberp sym))
(defun is-jsbool (sym)
  (or (equal sym :true)
      (equal sym :false)))

(defun is-jsstring (sym)
  (stringp sym))

(defun is-jslit (sym)
  (or (is-jsbool sym)
      (is-jsstring sym)
      (is-jsnum sym)))

(defun parse-literal (sym)
  (cond ((is-jsnum sym) (make-instance 'es-literal :value sym))
        ((is-jsbool sym) (make-instance 'es-literal :value sym))
        (t (make-instance 'es-literal :value (stringify sym)))))
