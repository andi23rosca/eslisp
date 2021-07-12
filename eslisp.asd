;;;; eslisp.asd

(asdf:defsystem #:eslisp
  :description "Describe eslisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria :fiveam :str)
  :components ((:file "package")
               (:file "helpers")
	           (:file "macros")
               (:file "eslisp-ast")
               (:file "eslisp")
               (:file "parser")
               (:file "eslisp-ast.spec")))
