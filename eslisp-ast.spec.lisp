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
(defvar fn-decl
  (make-instance 'es-function-declaration
                 :async t
                 :generator t
                 :body fn-body
                 :params (list iden)
                 :id fn-iden))


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
             (es->js fn-body)))
  (is (equal "async function* myFn(myVar){
  {
    myVar;
  }
}"
             (es->js fn-decl)))

  (is (equal "return;"
             (es->js (make-instance 'es-return-statement))))
  (is (equal "return 23.7;"
             (es->js (make-instance 'es-return-statement
                                    :argument num-lit))))
  (is (equal "break;"
             (es->js (make-instance 'es-break-statement))))
  (is (equal "continue;"
             (es->js (make-instance 'es-continue-statement)))))
