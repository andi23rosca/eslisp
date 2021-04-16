# eslisp
A common lisp macro that takes a lispified javascript DSL and converts it to javascript code.

## Examples:
```
(eslisp (let (my-var 2)
        (my-other-var)))
```
turns to
```js
let myVar = 2;
let myOtherVar;
```
