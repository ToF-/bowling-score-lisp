
## Quoting value
We have a list that is bound to the variable `numbers`. Can we call the function `+` on `numbers` ?
```lisp
* (+ numbers)

debugger invoked on a TYPE-ERROR @52A8AD60 in thread
#<THREAD "main thread" RUNNING {10044A0113}>:
  The value
    (42 17 23 4807)
  is not of type
    NUMBER

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

(+ (42 17 23 4807))
0]
```
We made a mistake, because the `+` function wants arguments that are numbers, and we gave it a list. Want we really want is to _apply_ the `+` function to our list. But writing this:
```lisp
* (apply + numbers)
```
will not work, because the second element of this list will be evaluated before being given to `apply` which will result in a mistake. We need a way to mention a lisp form *without* evaluating it, and that's what `quote` is for.

Remember that `foo` was defined as a variable:
```lisp
* foo
42
* (quote foo)
FOO
```
Thus, to apply a function to our numbers, we can write:
```lisp
* (apply (quote +) numbers)
4889
```
`quote` can be used on anything, not only functions:
```lisp
* (apply (quote +) (quote (1 2 3 4 5)))
15
```
Also, a shortcut for `quote` is to use the tick `'`:
```lisp
* (apply '* '(1 2 3 4 5))
120
```




