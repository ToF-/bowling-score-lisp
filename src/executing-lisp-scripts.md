## Executing lisp scripts

To execute a script written in lisp with sbcl, launch sbcl with the `--script` option. Suppose we have a script file name `my-script.lisp` that prints the result of an addition.

```lisp
; my-script.lisp  prints the result of an addition
(format t "~a~%" (+ 23 17 2))
```

Then this script can be executed as a stand-alone program:
```
> sbcl --script my-script.lisp
42
```

Careful, though: the `--script` option *bypasses* the loading of init files. That means that if we intend to use a script lisp as a standalone program that requires some common lisp library package, we will have to explicitely load the `quicklisp` library first in our script.
