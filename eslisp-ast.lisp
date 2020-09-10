(in-package :eslisp-ast)


(defun indent-string (str)
  (let ((lns (lines str)))
    (join "~%"
          (mapcar (lambda (s) (concat "  " s))
                  lns))))
(defun newline ()
  (format nil "~%"))

(export 'es->js)
(defgeneric es->js (es)
  (:documentation "Transforms es classes into javascript strings"))

(export 'defes)
(defmacro defes (name parents slots)
  `(progn
     (export ',(concatenate 'list (list name) (mapcar (lambda (s) (car s)) slots)))
     (defclass ,name ,parents
       ,(mapcar (lambda (slot)
                  (concatenate 'list
                               slot
                               (list :initarg (make-keyword (car slot)))
                               (list :accessor (car slot))))
         slots))))


;;NODE
(defclass es-node () ())


;;PATTERN
(defclass es-pattern (es-node) ())


;;EXPRESSION
(defclass es-expression (es-node) ())


;;FUNCTION
(export '(es-function id params body generator async))
(defclass es-function (es-node)
  ((id :initarg :id
       :accessor id
       :documentation "es-identifier | null")
   (params :initarg :params
           :accessor params
           :documentation "es-pattern[]")
   (body :initarg :body
         :accessor body
         :initform (error "Function must have a body.")
         :documentation "es-function-body")
   (generator :initarg :generator
              :accessor generator
              :documentation "boolean - If function is a generator.")
   (async :initarg :async
          :accessor async
          :documentation "boolean - If function is async.")))

;;ARROW FUNCTION EXPRESSION TODO


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
(export '(es-block-statement body))
(defclass es-block-statement (es-statement)
  ((body :initarg :body
         :accessor body
         :documentation "es-statement[] - Body made up of statements")))
(defmethod es->js ((es es-block-statement))
  (with-accessors ((body body)) es
    (let* ((converted (mapcar #'es->js body))
           (block-string (str:join "~%" converted)))
      (format nil "{~%~a~%}" (indent-string block-string)))))

;;FUNCTION BODY
(export '(es-function-body))
(defclass es-function-body (es-block-statement) ())


;;DEBUGGER STATEMENT
(export '(es-debugger-statement))
(defclass es-debugger-statement (es-statement) ())
(defmethod es->js ((es es-debugger-statement))
  "debugger;")


;;RETURN STATEMENT
(export '(es-return-statement argument))
(defclass es-return-statement (es-statement)
  ((argument :initarg :argument
             :accessor argument
             :documentation "es-expression | null - What to return.")))
(defmethod es->js ((es es-return-statement))
  (with-accessors ((argument argument)) es
    (if argument
        (concat "return " (es->js argument) ";")
        "return;")))


;;LABELED STATEMENT TODO


;;BREAK STATEMENT
(export '(es-break-statement label))
(defclass es-break-statement (es-statement)
  ((label :initarg :label
          :accessor label
          :documentation "es-identifier | null")))
(defmethod es->js ((es es-break-statement))
  (with-accessors ((label label)) es
    (concat "break " (es->js label) ";")))


;;CONTINUE STATEMENT
(export '(es-continue-statement label))
(defclass es-continue-statement (es-statement)
  ((label :initarg :label
          :accessor label
          :documentation "es-identifier | null")))
(defmethod es->js ((es es-continue-statement))
  (with-accessors ((label label)) es
    (concat "continue " (es->js label) ";")))


;;IF STATEMENT
(export '(es-if-statement test consequent alternate))
(defclass es-if-statement (es-statement)
  ((test :initarg :test
         :accessor test
         :initform (error "Must have a test")
         :documentation "es-expression - The if test")
   (consequent :initarg :consequent
               :accessor consequent
               :initform (error "Must have a consequent")
               :documentation "es-statement - Evaluated when test is true.")
    (alternate :initarg :alternate
               :accessor alternate
               :documentation "es-statement | null - Evaluated when test is false.")))
(defmethod es->js ((es es-if-statement))
  (with-accessors ((test test) (consequent consequent) (alternate alternate)) es
    (concat "if (" (es->js test) ") "
            (es->js consequent)
            (when alternate
              (concat (newline) "else " (es->js alternate))))))


;;SWITCH STATEMENT TODO


;;THROW STATEMENT
(export '(es-throw-statement argument))
(defclass es-throw-statement (es-statement)
  ((argument :initarg argument
             :accessor argument
             :initform (error "Must have an argument.")
             :documentation "es-expression")))
(defmethod es->js ((es es-throw-statement))
  (with-accessors ((argument argument)) es
    (concat "throw " (es->js argument))))


;;TRY STATEMENT TODO


;;CATCH CLAUSE TODO


;;WHILE STATEMENT
(export '(es-while-statement test body))
(defclass es-while-statement (es-statement)
  ((test :initarg :test
         :accessor test
         :initform (error "Must have a test")
         :documentation "es-expression")
   (body :initarg :body
               :accessor body
               :initform (error "Must have a body")
         :documentation "es-statement - Executed while test is true.")))
(defmethod es->js ((es es-while-statement))
  (with-accessors ((test test) (body body)) es
    (concat "while(" (es->js test) ")" (es->js body))))


;;DO WHILE STATEMENT
(export '(es-do-while-statement test body))
(defclass es-do-while-statement (es-statement)
  ((test :initarg :test
         :accessor test
         :initform (error "Must have a test")
         :documentation "es-expression")
   (body :initarg :body
               :accessor body
               :initform (error "Must have a body")
         :documentation "es-statement - Executed while test is true.")))
(defmethod es->js ((es es-do-while-statement))
  (with-accessors ((test test) (body body)) es
    (concat "do " (es->js body) " while(" (es->js test) ")")))


;;FOR STATEMENT TODO
;;FOR IN STATEMENT TODO
;;FOR OF STATEMENT TODO


;;DECLARATION
(defclass es-declaration (es-statement) ())


;;FUNCTION DECLARATION
(export '(es-function-declaration))
(defclass es-function-declaration (es-declaration es-function)
  ((id :initarg :id
       :accessor id
       :initform (error "Must have an id.")
       :documentation "es-identifier")))
;TODO add es->js 


;;VARIABLE DECLARATION

(defclass es-variable-declaration (es-declaration)
  ((declarations :initarg :declarations
                 :accessor declarations
                 :initform )))
