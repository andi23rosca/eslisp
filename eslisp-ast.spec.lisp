(in-package :eslisp-ast)

(def-suite js-conversion :description "Checks ast -> js conversion code.")

(in-suite js-conversion)

;;DEFINITIONS
(defvar iden
  (make-instance 'es-identifier
                 :name "myVar"))
(defvar fn-iden
  (make-instance 'es-identifier
                 :name "myFn"))

(defvar str-lit
  (make-instance 'es-literal
                 :value "a string"))
(defvar num-lit
  (make-instance 'es-literal
                 :value 23.7))
(defvar bool-lit-true
  (make-instance 'es-literal
                 :value :true))
(defvar bool-lit-false
  (make-instance 'es-literal
                 :value :false))

(defvar this-expr
  (make-instance 'es-this-expression))

(defvar expr-stm
  (make-instance 'es-expression-statement
                 :expression iden))
(defvar blck-stm
  (make-instance 'es-block-statement
                 :body (list expr-stm)))
(defvar fn-body
  (make-instance 'es-function-body
                 :body (list blck-stm)))


(fiveam:test identifier-tests
  "Identifier"
  (is (equal "myVar" (es->js iden)))
  (is (equal "myFn" (es->js fn-iden))))

(fiveam:test literals-test
  "Literals"
  (is (equal "'a string'" (es->js str-lit)))
  (is (equal "23.7" (es->js num-lit)))
  (is (equal "true" (es->js bool-lit-true)))
  (is (equal "false" (es->js bool-lit-false))))

(fiveam:test statements-test
  "Statements & Expressions"
  (is (equal "this" (es->js this-expr)))
  (is (equal "myVar;" (es->js expr-stm)))
  (is (equal "{
  myVar;
}"
             (es->js blck-stm)))
  (is (equal "{
  {
    myVar;
  }
}"
             (es->js fn-body))))
