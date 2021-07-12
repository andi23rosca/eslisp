# eslisp
A common lisp macro that takes a lispified javascript DSL and converts it to javascript code.

## Examples:

### Naming
Naming conventions are inspired by parenscript:

``` common-lisp
my-test-var ;; Becomes myTestVar
*my-test-var ;; Becomes MyTestVar
*my-test-var* ;; Becomes MY_TEST_VAR
```

For global js objects [found here](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects) 
use special keywords found in `/parser/parse-symbol.lisp` in the `*global-symbols*` plist.

As an example:

``` common-lisp
:encode-uri-component ;; Becomes EncodeURIComponent
:json ;; Becomes JSON
```


### Declarations

```
(eslisp (let (my-var 2)
             (my-other-var)))
```
turns to
```js
let myVar = 2;
let myOtherVar;
```

``` common-lisp
(eslisp (const (my-var 3)))
```
to

``` js
const myVar = 3;
```

### Chaining

No JS code is complete withouth being able to chain properties and functions with `.`.

``` common-lisp
(@ my-obj my-prop) ;; Becomes myObj.myProp
(@ my-obj (my-fn 3)) ;; Becomes myObj.myFn(3)
(@ my-obj 1) ;; Becomes myObj[1]
(@ my-obj "some string") ;; Becomes myObj["some string"]
```

To force a chaining to use the `[]` notation instead of `.` tag it with `:expr` like so:

``` common-lisp
(@ my-obj (:expr my-prop)) ;; Becomes myObj[myProp]
(@ my-obj (:expr (my-fn 3))) ;; Becomes myObj[myFn(3)]
```

