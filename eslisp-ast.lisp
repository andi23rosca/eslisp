(in-package :eslisp-ast)

(defun newline ()
  (format nil "~%"))


(defun indent-string (str)
  (let ((lns (lines str)))
    (join (newline)
          (mapcar (lambda (s) (concat "  " s))
                  lns))))


(export 'es->js)
(defgeneric es->js (es)
  (:documentation "Transforms es classes into javascript strings"))

(export 'defes)
(defmacro defes (name parents slots)
  "Defines a class and exports its name and slots"
  `(progn
     (export ',(concatenate 'list (list name) (mapcar (lambda (s) (car s)) slots)))
     (defclass ,name ,parents
       ,(mapcar (lambda (slot)
                  (concatenate 'list
                               slot
                               (list :initarg (make-keyword (car slot)))
                               (list :accessor (car slot))))
         slots))))

(defun filter-class-type (list type)
  (remove-if-not (lambda (x) (equal (type-of x) type)) list))

;Default catchall
(defmethod es->js ((es t))
  "")

;;NODE
(defclass es-node () ())


;;PATTERN
(defclass es-pattern (es-node) ())


;;EXPRESSION
(defclass es-expression (es-node) ())


;;FUNCTION
(defes es-function (es-node)
  ((id :initform nil
       :documentation "es-identifier | null")
   (params :initform nil
           :documentation "es-pattern[]")
   (body :initform (error "Function must have a body.")
         :documentation "es-function-body")
   (generator :initform nil
              :documentation "boolean - If function is a generator.")
   (async :initform nil
          :documentation "boolean - If function is async.")
   (shorthand :initform nil
              :documentation "boolean - shorthand for function expressions: myFn(){}.")))
(defmethod es->js ((es es-function))
  (with-accessors ((id id) (body body) (params params) (generator generator) (async async) (shorthand shorthand)) es
    (concat (when async "async ")
            (when (not shorthand)
              (if generator
                  "function* "
                  "function "))
            (when id (es->js id))
            "(" (join ", " (mapcar #'es->js params)) ") "
            (es->js body))))

;;ARROW FUNCTION EXPRESSION
(defes es-arrow-function-expression (es-function es-expression)
  ((body :initform (error "Must have a body")
         :documentation "es-function-body | es-expression")))
(defmethod es->js ((es es-arrow-function-expression))
  (with-accessors ((body body) (params params) (async async)) es
    (concat (when async "async ")
            "(" (join ", " (mapcar #'es->js params)) ") => "
            (if (equal (type-of body) 'es-object-expression)
                (concat "(" (es->js body) ")")
                (es->js body)))))


;;STATEMENT
(defclass es-statement (es-node) ())


;;EXPRESSION STATEMENT
(defes es-expression-statement (es-statement)
  ((expression :initform (error "Must have an expression defined.")
               :documentation "es-expression - The expression of the statement.")))
(defmethod es->js ((es es-expression-statement))
  (with-accessors ((expression expression)) es
    (concat (es->js expression) ";")))


;;THIS EXPRESSION
(export '(es-this-expression))
(defclass es-this-expression (es-expression) ())
(defmethod es->js ((es es-this-expression))
  "this")


;;IDENTIFIER
(defes es-identifier (es-expression es-pattern)
  ((name :initform (error "Identifier must have a name")
         :documentation "string")))
(defmethod es->js ((es es-identifier))
  (with-accessors ((name name)) es
    name))


;;LITERAL
(defes es-literal (es-expression)
  ((value :initform nil
          :documentation "string | boolean | null | number")))
(defmethod es->js ((es es-literal))
  (with-accessors ((value value)) es
    (cond ((stringp value) (concatenate 'string "'" value "'"))
          ((numberp value) (write-to-string value))
          ((equal value :true) "true")
          ((equal value :false) "false")
          (t "null"))))


;;BLOCK STATEMENT
(defes es-block-statement (es-statement)
  ((body :initform nil
         :documentation "es-statement[] - Body made up of statements")))
(defmethod es->js ((es es-block-statement))
  (with-accessors ((body body)) es
    (let* ((converted (mapcar #'es->js body))
           (block-string (join (newline) converted)))
      (concat "{" (newline)
              (indent-string block-string)
              (newline) "}"))))

;;FUNCTION BODY
(export '(es-function-body))
(defclass es-function-body (es-block-statement) ())


;;DEBUGGER STATEMENT
(export '(es-debugger-statement))
(defclass es-debugger-statement (es-statement) ())
(defmethod es->js ((es es-debugger-statement))
  "debugger;")


;;RETURN STATEMENT
(defes es-return-statement (es-statement)
  ((argument :initform nil
             :documentation "es-expression | null - What to return.")))
(defmethod es->js ((es es-return-statement))
  (with-accessors ((argument argument)) es
    (if argument
        (concat "return " (es->js argument) ";")
        "return;")))


;;LABELED STATEMENT TODO


;;BREAK STATEMENT
(defes es-break-statement (es-statement)
  ((label :initform nil
          :documentation "es-identifier | null")))
(defmethod es->js ((es es-break-statement))
  (with-accessors ((label label)) es
    (concat "break"
            (when label
              (concat " " (es->js label)))
            ";")))


;;CONTINUE STATEMENT
(defes es-continue-statement (es-statement)
  ((label :initform nil
          :documentation "es-identifier | null")))
(defmethod es->js ((es es-continue-statement))
  (with-accessors ((label label)) es
    (concat "continue"
            (when label
              (concat " " (es->js label)))
            ";")))


;;IF STATEMENT
(defes es-if-statement (es-statement)
  ((test :initform (error "Must have a test")
         :documentation "es-expression - The if test")
   (consequent :initform (error "Must have a consequent")
               :documentation "es-statement - Evaluated when test is true.")
    (alternate :documentation "es-statement | null - Evaluated when test is false.")))
(defmethod es->js ((es es-if-statement))
  (with-accessors ((test test) (consequent consequent) (alternate alternate)) es
    (concat "if (" (es->js test) ") "
            (es->js consequent)
            (when alternate
              (concat (newline) "else " (es->js alternate))))))


;;SWITCH STATEMENT
(defes es-switch-statement (es-statement)
  ((discriminant :initform (error "Must have a discriminant.")
                 :documentation "es-expression")
   (cases :documentation "es-switch-case[]")))
(defmethod es->js ((es es-switch-statement))
  (with-accessors ((discriminant discriminant) (cases cases)) es
    (concat "switch (" (es->js discriminant) ")" "{" (newline)
            (indent-string (join (newline)
                                 (mapcar #'es->js cases)))
            (newline) "}")))

;;SWITCH CASE
(defes es-switch-case (es-node)
  ((test :initform nil
         :documentation "es-expression | null")
   (consequent :initform nil
               :documentation "es-statement[]")))
(defmethod es->js ((es es-switch-case))
  (with-accessors ((test test) (consequent consequent)) es
    (concat "case " (if test (es->js test) "default") ":" (newline)
            (indent-string (join (newline) (mapcar #'es->js consequent))))))


;;THROW STATEMENT
(defes es-throw-statement (es-statement)
  ((argument :initform (error "Must have an argument.")
             :documentation "es-expression")))
(defmethod es->js ((es es-throw-statement))
  (with-accessors ((argument argument)) es
    (concat "throw " (es->js argument))))


;;TRY STATEMENT
(defes es-try-statement (es-statement)
  ((eblock :initform (error "Must have a block.")
           :documentation "es-block-statement")
   (handler :initform nil
            :documentation "es-catch-clause | null")
   (finalizer :initform nil
              :documentation "es-block-statement | null")))
(defmethod es->js ((es es-try-statement))
  (with-accessors ((eblock eblock) (handler handler) (finalizer finalizer)) es
    (concat "try " (es->js eblock) (newline)
            (when handler (concat (es->js handler) (newline)))
            (when finalizer (concat "finally " (es->js handler))))))


;;CATCH CLAUSE
(defes es-catch-clause (es-node)
  ((param :initform nil
          :documentation "es-pattern | null")
   (body :initform (error "Must have a body.")
         :documentation "es-block-statement")))
(defmethod es->js ((es es-catch-clause))
  (with-accessors ((param param) (body body)) es
    (concat "catch " (when param (concat "(" (es->js param) ") "))
            (es->js body))))


;;WHILE STATEMENT
(defes es-while-statement (es-statement)
  ((test :initform (error "Must have a test")
         :documentation "es-expression")
   (body :initform (error "Must have a body")
         :documentation "es-statement - Executed while test is true.")))
(defmethod es->js ((es es-while-statement))
  (with-accessors ((test test) (body body)) es
    (concat "while(" (es->js test) ")" (es->js body))))


;;DO WHILE STATEMENT
(defes es-do-while-statement (es-statement)
  ((test :initform (error "Must have a test")
         :documentation "es-expression")
   (body :initform (error "Must have a body")
         :documentation "es-statement - Executed while test is true.")))
(defmethod es->js ((es es-do-while-statement))
  (with-accessors ((test test) (body body)) es
    (concat "do " (es->js body) " while(" (es->js test) ")")))


;;FOR STATEMENT
(defes es-for-statement (es-statement)
  ((init :initform nil
         :documentation "es-variable-declaration | es-expression | null")
   (test :initform nil
         :documentation "es-expression | null")
   (update :initform nil
           :documentation "es-expression | null")
   (body :initform "Must have a body."
         :documentation "es-statement")))
(defmethod es->js ((es es-for-statement))
  (with-accessors ((init init) (test test) (update update) (body body)) es
    (concat "for ("
            (when init (es->js init)) ";"
            (when test (es->js test)) ";"
            (when update (es->js update))
            ") "
            (es->js body))))


;;FOR IN STATEMENT
(defes es-for-in-statement (es-statement)
  ((left :initform (error "Must have a left side.")
         :documentation "es-variable-declaration | es-pattern")
   (right :initform (error "Must have a right side.")
          :documentation "es-expression")
   (body :initform (error "Must have a body.")
         :documentation "es-statement")))
(defmethod es->js ((es es-for-in-statement))
  (with-accessors ((left left) (right right) (body body)) es
    (concat "for (" (es->js left) " in " (es->js right) ")"
            (es->js body))))


;;FOR OF STATEMENT
(defes es-for-of-statement (es-for-in-statement)
  ((await :initform nil
          :documentation "boolean")))
(defmethod es->js ((Es es-for-of-statement))
  (with-accessors ((left left) (right right) (body body) (await await)) es
    (concat "for " (when await "await") "("
            (es->js left) " in " (es->js right) ")"
            (es->js body))))


;;DECLARATION
(defclass es-declaration (es-statement) ())


;;FUNCTION DECLARATION
(defes es-function-declaration (es-declaration es-function)
  ((id :initform nil
       :documentation "es-identifier")))


;;VARIABLE DECLARATION
(defes es-variable-declaration (es-declaration)
  ((declarations :initform nil
                 :documentation "es-variable-declarator[]")
   (kind :initform (error "Must have a kind.")
         :documentation ":var | :let | :const")))
(defmethod es->js ((es es-variable-declaration))
  (with-accessors ((declarations declarations) (kind kind)) es
    (join (newline)
          (mapcar (lambda (dec)
                    (concat (string-downcase kind) " "
                            (es->js dec) ";"))
                  declarations))))


;;VARIABLE DECLARATOR
(defes es-variable-declarator (es-node)
  ((id :initform (error "Must have an id.")
       :documentation "es-pattern")
   (init :initform nil
         :documentation "es-expression | null")))
(defmethod es->js ((es es-variable-declarator))
  (with-accessors ((id id) (init init)) es
    (concat (es->js id)
            (when init
              (concat " = " (es->js init))))))


;;ARRAY EXPRESSION
(defes es-array-expression (es-expression)
  ((elements :initform nil
             :documentation "(es-expression | es-spread-element | null)[]")))
(defmethod es->js ((es es-array-expression))
  (with-accessors ((elements elements)) es
    (concat "[" (join ", " (mapcar #'es->js elements)) "]")))


;;OBJECT EXPRESSION
(defes es-object-expression (es-expression)
  ((properties :initform nil
               :documentation "(es-property | es-spread-element)[]")))
(defmethod es->js ((es es-object-expression))
  (let* ((delimiter (if (> (length (properties es)) 2) (newline) " "))
         (props (join (concat "," delimiter)
                      (mapcar #'es->js (properties es)))))
    (concat "{" delimiter
            (if (equal " " delimiter)
                props
                (indent-string props))
            delimiter "}")))


;;PROPERTY
(defes es-property (es-node)
  ((key :initform (error "Must have a key.")
        :documentation "es-literal | es-identifier | es-expression")
   (value :initform nil
          :documentation "es-expression")
   (kind :initform "init"
         :documentation "'init' | 'get' | 'set'")
   (emethod :initform nil
           :documentation "boolean")
   (shorthand :initform nil
              :documentation "boolean - of form { key }")
   (computed :initform nil
             :documentation "boolean - of form {[key]: value}")))
(defmethod es->js ((es es-property))
  (with-accessors ((key key)
                   (value value)
                   (kind kind)
                   (emethod emethod)
                   (shorthand shorthand)
                   (computed computed)) es
    (if (equal kind "init") ;;TODO Handle method / function
        (cond (emethod (concat (es->js key) ": " (es->js value)))
              (shorthand (es->js key))
              (computed (concat "[" (es->js key) "]: " (es->js value)))
              (t (concat (es->js key) ": " (es->js value))))
        (concat kind " " (es->js key)))))


;;FUNCTION EXPRESSION
(defes es-function-expression (es-function es-expression)
  ((shorthand :initform t)))

;;YIELD EXPRESSION
(defes es-yield-expression (es-expression)
  ((argument :initform nil
             :documentation "es-expression | null")
   (delegate :initform nil
             :documentation "boolean")))
(defmethod es->js ((es es-expression))
  (with-accessors ((argument argument) (delegate delegate)) es
    (concat (if delegate
                "yield* "
                "yield ")
            (es->js argument)
            ";")))


;;AWAIT EXPRESSION
(defes es-await-expression (es-expression)
  ((argument :initform (error "Must have an argument.")
             :documentation "es-expression")))
(defmethod es->js ((es es-await-expression))
  (with-accessors ((argument argument)) es
    (concat "await " (es->js argument))))


;;SUPER EXPRESSION
(defes es-super-expression (es-node) ())
(defmethod es->js ((es es-super-expression))
  "super")


;;SPREAD ELEMENT
(defes es-spread-element (es-node)
  ((argument :initform (error "Must have an argument.")
             :documentation "es-expression")))
(defmethod es->js ((es es-spread-element))
  (concat "..." (es->js (argument es))))


;;UNARY EXPRESSION
(defes es-unary-expression (es-expression)
  ((operator :initform (error "Must have an operator.")
             :documentation "'-' | '+' | '!' | '~' | 'typeof' | 'void' | 'delete'")
   (prefix :initform nil
           :documentation "boolean")
   (argument :initform (error "Must have an argument.")
             :documentation "es-expression")))
(defmethod es->js ((es es-unary-expression))
  (with-accessors ((operator operator) (prefix prefix) (argument argument)) es
    (concat operator (when (not prefix) " ") (es->js argument))))


;;UPDATE EXPRESSION
(defes es-update-expression (es-expression)
  ((operator :initform (error "Must have an operator.")
             :documentation "'++' | '--'")
   (prefix :initform nil
           :documentation "boolean")
   (argument :initform (error "Must have an argument.")
             :documentation "es-expression")))
(defmethod es->js ((es es-update-expression))
  (with-accessors ((operator operator) (prefix prefix) (argument argument)) es
    (concat (when prefix operator) (es->js argument) (when (not prefix) operator))))

;;BINARY EXPRESSION
(defes es-binary-expression (es-expression)
  ((operator :initform (error "Must have an operator.")
             :documentation "TODO add operators")
   (left :initform (error "Must have a left side.")
         :documentation "es-expression")
   (right :initform (error "Must have a right side.")
          :documentation "es-expression")))
(defmethod es->js ((es es-binary-expression))
  (with-accessors ((operator operator) (left left) (right right)) es
    (concat (es->js left) " " operator " " (es->js right))))


;;ASSIGNMENT EXPRESSION
(defes es-assignment-expression (es-expression)
  ((operator :initform (error "Must have an operator.")
             :documentation "TODO add operators")
   (left :initform (error "Must have a left side.")
         :documentation "es-expression")
   (right :initform (error "Must have a right side.")
          :documentation "es-pattern | es-expression")))
(defmethod es->js ((es es-assignment-expression))
  (with-accessors ((operator operator) (left left) (right right)) es
    (concat (es->js left) " " operator " " (es->js right))))


;;LOGICAL EXPRESSION
(defes es-logical-expression (es-expression)
  ((operator :initform (error "Must have an operator.")
             :documentation "|| | && | ??")
   (left :initform (error "Must have a left side.")
         :documentation "es-expression")
   (right :initform (error "Must have a right side.")
          :documentation "es-expression")))
(defmethod es->js ((es es-logical-expression))
  (with-accessors ((operator operator) (left left) (right right)) es
    (concat (es->js left) " " operator " " (es->js right))))


;;MEMBER EXPRESSION
(defes es-member-expression (es-expression es-pattern)
  ((object :initform (error "Must have an object.")
           :documentation "es-expression | es-super")
   (property :initform (error "Must have a property.")
             :documentation "es-expression | es-identifier")
   (computed :initform nil
             :documentation "boolean - When true of form a[b] else a.b")
   (optional :initform nil
             :documentation "boolean - optional chaining: a.b?")))
(defmethod es->js ((es es-member-expression))
  (with-accessors ((object object) (property property) (computed computed) (optional optional)) es
    (concat (if (equal (type-of object) 'es-member-expression)
                (concat "(" (es->js object) ")")
                (es->js object))
            (if computed
                (concat "[" (es->js property) "]")
                (concat "." (es->js property)))
            (when optional "?"))))


;;CHAIN EXPRESSION TODO



;;CONDITIONAL EXPRESSION
(defes es-conditional-expression (es-expression)
  ((test :initform (error "Must have a test.")
         :documentation "es-expression")
   (alternate :initform (error "Must have alternate.")
              :documentation "es-expression")
   (consequent :initform (error "Must have a consequent.")
               :documentation "es-expression")))
(defmethod  es->js ((es es-conditional-expression))
  (with-accessors ((test test) (alternate alternate) (consequent consequent)) es
    (concat (es->js test) " ? " (es->js consequent) " : " (es->js alternate))))


;;CALL EXPRESSION
(defes es-call-expression (es-expression)
  ((callee :initform (error "Must have a callee.")
           :documentation "es-expression | es-super")
   (arguments :initform nil
              :documentation "(es-expression | es-spread-element)[]")))
(defmethod es->js ((es es-call-expression))
  (concat (es->js (callee es)) "("
          (join ", " (mapcar #'es->js (arguments es)))
          ")"))


;;NEW EXPRESSION
(defes es-new-expression (es-expression)
  ((callee :initform (error "Must have a callee.")
           :documentation "es-expression")
   (arguments :documentation "(es-expression | es-spread-element)[]")))
(defmethod es->js ((es es-new-expression))
  (with-accessors ((callee callee) (arguments arguments)) es
    (concat "new " (es->js callee)
            "(" (join ", " (mapcar #'es->js arguments)) ")")))


;;SEQUENCE EXPRESSION
(defes es-sequence-expression (es-expression)
  ((expressions :documentation "es-expression[]")))
(defmethod es->js ((es es-sequence-expression))
  (join ", " (mapcar #'es->js (expressions es))))


;;IMPORT EXPRESSION
(defes es-import-expression (es-expression)
  ((source :initform (error "Must have source.")
           :documentation "es-expression")))
(defmethod es->js ((es es-import-expression))
  (concat "import(" (es->js (source es)) ")"))


;;TEMPLATE LITERAL
(defes es-template-literal (es-expression)
  ((elements :initform nil
           :documentation "(es-template-element | es-expression)[]")))
(defmethod es->js ((es es-template-literal))
  (concat "`"
          (join "" (mapcar (lambda (el)
                             (if (equal (type-of el) 'es-template-element)
                                 (es->js el)
                                 (concat "${" (es->js el) "}")))
                           (elements es)))
          "`"))


;;TAGGED TEMPLATE EXPRESSION
(defes es-tagged-template-expression (es-expression)
  ((tag :initform (error "Must have a tag.")
        :documentation "es-expression")
   (quasi :initform (error "Must have a quasi")
          :documentation "es-template-literal")))
(defmethod es->js ((es es-tagged-template-expression))
  (concat (es->js (tag es)) (es->js (quasi es))))


;;TEMPLATE ELEMENT
(defes es-template-element (es-node)
  ((value :initform ""
          :documentation "string")))
(defmethod es->js ((es es-template-element))
  (value es))


;;OBJECT PATTERN
(defes es-object-pattern (es-object-expression) ())


;;ARRAY PATTERN
(defes es-array-pattern (es-array-expression) ())


;;ASSIGNMENT PATTERN
(defes es-assignment-pattern (es-pattern)
  ((left :initform (error "Must have a left side.")
         :documentation "es-pattern")
   (right :initform (error "Must have a right side.")
          :documentation "es-expression")))
(defmethod es->js ((es es-assignment-pattern))
  (concat (es->js (left es)) " = " (es->js (right es))))


;;CLASS
(defes es-class (es-node)
  ((id :initform nil
       :documentation "es-identifier | null")
   (super-class :initform nil
                :documentation "es-expression | null")
   (body :initform (error "Must have a body.")
         :documentation "es-class-body")))
(defmethod es->js ((es es-class))
  (with-accessors ((id id) (super-class super-class) (body body)) es
      (concat "class "
              (when id (es->js id))
              (when super-class (concat "extends " (es->js super-class)))
              " "
              (es->js body))))


;;CLASS BODY
(defes es-class-body (es-node)
  ((body :initform nil
         :documentation "es-method-definition")))
(defmethod es->js ((es es-class-body))
  (concat "{" (newline)
          (indent-string (join (newline)
                               (mapcar #'es->js (body es))))
          (newline) "}"))


;;METHOD DEFINITION
(defes es-method-definition (es-node)
  ((key :initform (error "Must have a key.")
        :documentation "es-expression")
   (value :initform (error "Must have a value.")
          :documentation "es-function-expression")
   (kind :initform "method"
         :documentation "constructor | method | get | set")
   (computed :initform nil
             :documentation "boolean")
   (static :initform nil
           :documentation "boolean")))
(defmethod es->js ((es es-method-definition))
  (with-accessors ((key key) (value value) (kind kind) (computed computed) (static static)) es
    (if (equal kind "method")
        (concat (es->js key)
                (es->js value))
        (concat (when static "static ")
                (es->js key)
                (es->js value)))))


;;CLASS DECLARATION
(defes es-class-declaration (es-class)
  ((id :initform (error "Must have an id."))))


;;CLASS EXPRESSION
(defes es-class-expression (es-class) ())


;;META PROPERTY TODO


;;MODULE DECLARATION
(defes es-module-declaration (es-node) ())


;;MODULE SPECIFIER
(defes es-module-specifier (es-node)
  ((local :initform (error "Must have local")
          :documentation "es-identifier")))


;;IMPORT DECLARATION
(defes es-import-declaration (es-module-declaration)
  ((specifiers :documentation "(es-import-specifier | es-import-default-specifier | es-import-namespace-specifier)[]")
   (source :initform (error "Must have a source.")
           :documentation "es-literal")))
(defmethod es->js ((es es-import-declaration))
  (with-accessors ((source source) (specifiers specifiers)) es
    (let ((defaults (filter-class-type specifiers 'es-import-default-specifier))
          (namespace (filter-class-type specifiers 'es-import-namespace-specifier))
          (normal (filter-class-type specifiers 'es-import-specifier)))
      (concat "import "
              (when namespace (es->js (car namespace)))
              (when defaults (concat (when namespace ", ") (es->js (car defaults))))
              (when normal (concat (when (or namespace defaults) ", ")
                                   "{" (join ", " (mapcar #'es->js normal)) "}"))
              " from " (es->js source)
              ";"))))


;;IMPORT SPECIFIER
(defes es-import-specifier (es-module-specifier)
  ((imported :documentation "es-identifier")))
(defmethod es->js ((es es-import-specifier))
  (with-accessors ((local local) (imported imported)) es
    (let ((l (es->js local))
          (i (es->js imported)))
      (if (equal l i)
          l
          (concat i " as " l)))))


;;IMPORT DEFAULT SPECIFIER
(defes es-import-default-specifier (es-module-specifier) ())
(defmethod es->js ((es es-import-declaration))
  (es->js (local es)))


;;IMPORT NAMESPACE SPECIFIER
(defes es-import-namespace-specifier (es-module-specifier) ())
(defmethod es->js ((es es-import-namespace-specifier))
  (concat "* as " (es->js (local es))))


;;EXPORT NAMED DECLARATION
(defes es-export-named-declaration (es-module-declaration)
  ((edeclaration :documentation "es-declaration | null")
   (specifiers :documentation "es-export-specifier[]")
   (source :documentation "es-literal | null")))
(defmethod es->js ((es es-export-named-declaration))
  (with-accessors ((edeclaration edeclaration) (specifiers specifiers) (source source)) es
    (cond (edeclaration (concat "export " (es->js edeclaration) ";"))
          (specifiers (concat "export {" (join ", " (mapcar #'es->js specifiers)) "}"
                              (when source (concat " from " (es->js source)))
                              ";")))))


;;EXPORT SPECIFIER
(defes es-export-specifier (es-module-specifier)
  ((exported :initform (error "Must have an exported.")
             :documentation "es-identifier")))
(defmethod es->js ((es es-export-specifier))
  (with-accessors ((local local) (exported exported)) es
    (let ((l (es->js local))
          (e (es->js exported)))
      (if (equal l e)
          l
          (concat e " as " l)))))


;;EXPORT DEFAULT DECLARATION
(defes es-export-default-declaration (es-module-declaration)
  ((edeclaration :initform (error "Must have a declaration.") ;Has to be named edeclaration so it doesn't conflict with cl lock.
                :Documentation "Es-function-declaration | es-class-declaration | es-expression")))
(defmethod es->js ((es es-export-default-declaration))
  (concat "export default " (es->js (edeclaration es)) ";"))


;;EXPORT All DECLARATION
(defes es-export-all-declaration (es-module-declaration)
  ((source :initform (error "Must have a source.")
           :documentation "es-literal")
   (exported :documentation "es-identifier | null")))
(defmethod es->js ((es es-export-all-declaration))
  (with-accessors ((source source) (exported exported)) es
    (concat "export * "
            (when exported "as " (concat (es->js exported) " "))
            "from " (es->js source) ";")))
