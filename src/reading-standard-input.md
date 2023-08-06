## Reading the Standard Input

The `read` function read any value from the standard input. Here's an example, the following expression reads a number and print its binary representation:
```lisp
> rlwrap sbcl
* (format T "~b" (read))
4807 ↵
1001011000111
NIL
*
```
Here we read a format string and a value, and then print the result:
```lisp
* (format T (read) (read))
"this is your result: ~f" ↵
4807 ↵
this is your result: 4807.0
NIL
```
