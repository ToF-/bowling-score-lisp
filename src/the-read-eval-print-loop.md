## The Read Eval Print Loop

Let's play a bit with `sbcl`.
```
> rlwrap sbcl
This is SBCL 2.3.4, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
*
```
Sbcl is waiting for an input from the user. 
```lisp
* 42
42
* 17
17
```
### Exiting the loop
Exiting sbcl is possible at any moment via the `exit` function. The way to call a function in lisp is to use parentheses:
```lisp
* (exit)
>
```
Let's restart `sbcl` and explore some other features.
