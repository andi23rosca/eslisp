(cl:in-package :eslisp)

(defparameter *global-symbols*
  '(:infinity "Infinity"
    :-infinity "-Infinity"
    :nan "NaN"
    :undefined "undefined"
    :global-this "globalThis"
    :eval "eval"
    :uneval "uneval"
    :is-finite "isFinite"
    :is-nan "isNaN"
    :parse-float "parseFloat"
    :parse-int "parseInt"
    :encode-uri "encodeURI"
    :encode-uri-component "encodeURIComponent"
    :decode-uri "decodeURI"
    :decode-uri-component "decodeURIComponent"
    :object "Object"
    :function "Function"
    :boolean "Boolean"
    :symbol "Symbol"
    :error "Error"
    :aggregate-error "AggregateError"
    :eval-error "EvalError"
    :internal-error "InternalError"
    :range-error "RangeError"
    :reference-error "ReferenceError"
    :syntax-error "SyntaxError"
    :type-error "TypeError"
    :uri-error "URIError"
    :number "Number"
    :big-int "BigInt"
    :math "Math"
    :date "Date"
    :string "String"
    :reg-exp "RegExp"
    :array "Array"
    :int-8-array "Int8Array"
    :uint-8-array "Uint8Array"
    :uint-8-clamped-array "Uint8ClampedArray"
    :int-16-array "Int16Array"
    :uint-16-array "Uint16Array"
    :int-32-array "Int32Array"
    :uint-32-array "Uint32Array"
    :float-32-array "Float32Array"
    :float-64-array "Float64Array"
    :big-int-64-array "BigInt64Array"
    :big-uint-64-array "BigUint64Array"
    :map "Map"
    :set "Set"
    :weak-map "WeakMap"
    :weak-set "WeakSet"
    :array-buffer "ArrayBuffer"
    :shared-array-buffer "SharedArrayBuffer"
    :atomics "Atomics"
    :data-view "DataView"
    :json "JSON"
    :promise "Promise"
    :generator "Generator"
    :generator-function "GeneratorFunction"
    :async-function "AsyncFunction"
    :async-generator "AsyncGenerator"
    :async-generator-function "AsyncGeneratorFunction"
    :reflect "Reflect"
    :proxy "Proxy"
    :intl "Intl"
    :intl "Intl"
    :intl-collator "Intl.Collator"
    :intl-date-time-format "Intl.DateTimeFormat"
    :intl-list-format "Intl.ListFormat"
    :intl-number-format "Intl.NumberFormat"
    :intl-plural-rules "Intl.PluralRules"
    :intl-relative-time-format "Intl.RelativeTimeFormat"
    :intl-locale "Intl.Locale"
    :web-assembly "WebAssembly"
    :web-assembly-module "WebAssembly.Module"
    :web-assembly-instance "WebAssembly.Instance"
    :web-assembly-memory "WebAssembly.Memory"
    :web-assembly-table "WebAssembly.Table"
    :web-assembly-compile-error "WebAssembly.CompileError"
    :web-assembly-link-error "WebAssembly.LinkError"
    :web-assembly-runtime-error "WebAssembly.RuntimeError"))

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
    (cond ((getf *global-symbols* sym) (getf *global-symbols* sym))
          ((should-be-uppercased str)  (let* ((underscored (replace-all "-" "_" str))
                                              (removed (replace-all "*" "" underscored)))
                                         (string-upcase removed)))
          ((alexandria:starts-with #\* str) (kebab-to-camel-case (replace-all "*" "-" str)))
          (t (kebab-to-camel-case str)))))


(defun parse-symbol (sym)
  (make-instance 'es-identifier :name (stringify-symbol sym)))
