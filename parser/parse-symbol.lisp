(cl:in-package :eslisp)

(defun split-by-dash (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position #\- string :start i)
        collect (subseq string i j)
        while j))

(defun capitalize-all-but-first (ls)
  "Capitalizes all strings in a list except for the first one"
  (cons (car ls)
        (mapcar #'string-capitalize (cdr ls))))

(defun kebab-to-camel-case (str)
  "Turns a kebab cased string into a camel case one.
'my-test-var' to 'myTestVar'"
  (let* ((splitted (split-by-dash str))
         (camelCased (capitalize-all-but-first splitted)))
    (apply #'concat camelCased)))

(defun should-be-uppercased (str)
  (and (alexandria:starts-with #\* str)
       (alexandria:ends-with #\* str)))

(defun stringify-symbol (sym)
  (let ((str (stringify sym)))
    (cond ((should-be-uppercased str)  (let* ((underscored (replace-all "-" "_" str))
                                              (removed (replace-all "*" "" underscored)))
                                         (string-upcase removed)))
          ((alexandria:starts-with #\* str) (kebab-to-camel-case (replace-all "*" "-" str)))
          (t (kebab-to-camel-case str)))))

(defun parse-symbol (sym)
  (make-instance 'es-identifier :name (stringify-symbol sym)))
