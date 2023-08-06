
## Documentation
You can get some basic info about a function using the `documentation` function:
```lisp
* (documentation '+ 'function)
"Return the sum of its arguments. With no args, returns 0."
```
Here the documentation about the `documentation` function itself:
```lisp
* (documentation 'documentation 'function)

"Return the documentation string of Doc-Type for X, or NIL if none
exists. System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE, SETF, and T.

Function documentation is stored separately for function names and objects:
DEFUN, LAMBDA, &co create function objects with the specified documentation
strings.

 (SETF (DOCUMENTATION NAME 'FUNCTION) STRING)

sets the documentation string stored under the specified name, and

 (SETF (DOCUMENTATION FUNC T) STRING)

sets the documentation string stored in the function object.

 (DOCUMENTATION NAME 'FUNCTION)

returns the documentation stored under the function name if any, and
falls back on the documentation in the function object if necessary."
```
