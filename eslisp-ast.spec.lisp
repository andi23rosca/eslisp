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
(defvar fn-expr
  (make-instance 'es-function-expression
                 :body fn-body
                 :params (list iden)))

(defvar arr-expr
  (make-instance 'es-array-expression
                 :elements (list num-lit iden)))
(defvar obj-expr
  (make-instance
   'es-object-expression
   :properties (list (make-instance
                      'es-property
                      :key iden
                      :shorthand t)
                     (make-instance
                      'es-property
                      :key num-lit
                      :computed t
                      :value bool-lit-true))))
(defvar temp-element
  (make-instance 'es-template-element
                 :value "test str:"))
(defvar temp-lit
  (make-instance 'es-template-literal
                 :elements (list temp-element
                                 iden)))

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
  (is (equal "async function* myFn(myVar) {
  {
    myVar;
  }
}"
             (es->js fn-decl)))
  (is (equal "(myVar) {
  {
    myVar;
  }
}"
             (es->js fn-expr)))

  (is (equal "return;"
             (es->js (make-instance 'es-return-statement))))
  (is (equal "return 23.7;"
             (es->js (make-instance 'es-return-statement
                                    :argument num-lit))))
  (is (equal "break;"
             (es->js (make-instance 'es-break-statement))))
  (is (equal "continue;"
             (es->js (make-instance 'es-continue-statement))))
  (is (equal "if (23.7) {
  myVar;
}
else 23.7"
             (es->js (make-instance 'es-if-statement
                                    :test num-lit
                                    :consequent blck-stm
                                    :alternate num-lit))))
  (is (equal "[23.7, myVar]"
             (es->js arr-expr)))
  (is (equal "{ myVar, [23.7]: true }"
             (es->js obj-expr)))
  (is (equal "...[23.7, myVar]"
             (es->js (make-instance 'es-spread-element
                                    :argument arr-expr))))
  (is (equal "!myVar"
             (es->js (make-instance 'es-unary-expression
                            :argument iden
                            :operator "!"
                            :prefix t))))
  (is (equal "typeof myVar"
             (es->js (make-instance 'es-unary-expression
                                    :argument iden
                                    :operator "typeof"))))
  (is (equal "++myVar"
             (es->js (make-instance 'es-update-expression
                                    :argument iden
                                    :operator "++"
                                    :prefix t))))
  (is (equal "myVar++"
             (es->js (make-instance 'es-update-expression
                                    :argument iden
                                    :operator "++"))))
  (is (equal "myVar + 23.7"
             (es->js (make-instance 'es-binary-expression
                                    :left iden
                                    :right num-lit
                                    :operator "+"))))
  (is (equal "myVar && 23.7"
             (es->js (make-instance 'es-logical-expression
                                    :left iden
                                    :right num-lit
                                    :operator "&&"))))
  (is (equal "myVar.myFn"
             (es->js (make-instance 'es-member-expression
                                    :object iden
                                    :property fn-iden))))
  (is (equal "myVar.myFn?"
             (es->js (make-instance 'es-member-expression
                                    :object iden
                                    :property fn-iden
                                    :optional t))))
  (is (equal "myVar[myFn]"
             (es->js (make-instance 'es-member-expression
                                    :object iden
                                    :property fn-iden
                                    :computed t))))
  (is (equal "myVar[myFn]?"
             (es->js (make-instance 'es-member-expression
                                    :object iden
                                    :property fn-iden
                                    :optional t
                                    :computed t))))
  (is (equal "myVar ? 23.7 : myFn"
             (es->js (make-instance 'es-conditional-expression
                                    :test iden
                                    :consequent num-lit
                                    :alternate fn-iden))))
  (is (equal "myFn(myVar, 23.7)"
             (es->js (make-instance 'es-call-expression
                                    :callee fn-iden
                                    :arguments (list iden num-lit)))))
  (is (equal "new myFn(myVar, 23.7)"
             (es->js (make-instance 'es-new-expression
                                    :callee fn-iden
                                    :arguments (list iden num-lit)))))
  (is (equal "`test str:${myVar}`"
             (es->js temp-lit)))
  (is (equal "myFn`test str:${myVar}`"
             (es->js (make-instance 'es-tagged-template-expression
                                    :quasi temp-lit
                                    :tag fn-iden)))))
