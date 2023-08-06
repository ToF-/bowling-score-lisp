## Printing Values

Printing any value in lisp is quite easy, although it involves using a function with a lot of possible options, `format`. This function takes a _destination_ argument, a _format_ argument, and a (possibly empty) list of values.

The destination can be a dynamic string, a file, the standard output stream, or a simple string constant returned by the function. Let's interest ourselves in the two last possiblities. If the destination argument is `T` (the symbol for boolean value true) the result will be printed on the standard output (and the function will return NIL). If the destination argument is `NIL` (false) the function will return the formatted string.

The `"~a"` format argument allows for any lisp value. Other formats are possible.

```lisp
> rlwrap sbcl
* (format T "~a" 4807)
4807
NIL
* (format NIL "~a" 4807)
"4807"
* (format NIL "~x" 4807)
"12C7"
* (format NIL "~b" 4807)
"1001011000111"
* (format NIL "~f" 4807)
"4807.0"
* (format NIL "~f" 4807)
```

Writing a program that would print a numerical result is straightforward. Here's an exemple.
```lisp
; my-result.lisp

(format T "~a~d" (* 2 3 4 5 6 7 8 9 10))
```
The `~%` modifier means a newline has to be printed.
```
> sbcl --script my-result.lisp
3628800
```

