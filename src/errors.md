## Errors

Making a mistake is very easy in sbcl. Here's one:
```lisp
* foo

debugger invoked on a UNBOUND-VARIABLE @52B57214 in thread
#<THREAD "main thread" RUNNING {10044A0113}>:
  The variable FOO is unbound.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE   ] Retry using FOO.
  1: [USE-VALUE  ] Use specified value.
  2: [STORE-VALUE] Set specified value and use it.
  3: [ABORT      ] Exit debugger, returning to top level.

(SB-INT:SIMPLE-EVAL-IN-LEXENV FOO #<NULL-LEXENV>)
0]
```
No we are in the debugger. The best is to abort:
```lisp
0] abort
*
```
But we could have defined the value:
```lisp
* foo
…
0] store-value

Enter a form to be evaluated: 42
42
* foo
42
```

That being said, the best way to have `foo` mean something is to define it as a variable in the first place:
```lisp
* (defvar foo 42)
FOO
* foo
42
```
