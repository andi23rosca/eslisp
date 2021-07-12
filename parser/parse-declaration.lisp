(in-package :eslisp)


(defun parse-declarator (dec)
  (make-instance 'es-variable-declarator
                 :id (parse-eslisp (car dec))
                 :init (when (cadr dec)
                         (parse-eslisp (cadr dec)))))
(defun parse-declaration (expr)
  (let ((kind (stringify (car expr)))
        (vars (cdr expr)))
    ;Using container to spread out declarations as different statements.
    (make-instance 'es-container
                   :items (mapcar
                           (lambda (dec)
                             (make-instance 'es-variable-declaration
                                            :kind kind
                                            :declarations (list (parse-declarator dec))))
                           vars))))


(fiveam:test parse-declaration-test
  "Parse declaration"
  (is (equal (ast->javascript (list (parse-declaration
                                     '(let (my-var 1)
                                       (*my-class :true)
                                       (*my-const* "asd")
                                       (test something)))))
             '("let myVar = 1;
let MyClass = true;
let MY_CONST = 'asd';
let test = something;"))))
