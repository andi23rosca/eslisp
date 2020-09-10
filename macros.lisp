(in-package :eslisp)


(defun expand-all (body)
  "Expands all macros in a body"
  (let ((expanded (macroexpand body)))
    (if (listp expanded)
	(loop :for b :in expanded :collect (expand-all b))
	expanded)))
