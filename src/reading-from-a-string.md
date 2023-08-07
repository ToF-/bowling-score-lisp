## Reading From a String

The `read` function is truly powerful. It can actually read from any _stream_, be it the standard input stream, an opened file, or a stream formed with a given string.

The function `with-input-from-string` allow us to 
- define a named stream
- use it in a form as a source stream for `read`

Here are examples:
```lisp
* (with-input-from-string (s "42") (read s))
42
* (with-input-from-string (s  "42 17") (cons (read s) (cons (read s) NIL)))
(42 17)
```

As seen in the section about [creating lists](./creating-lists.md), `cons` and recursion can help us a lot!

In the next chapter we are going to assemble all that we have seen here to write the main function of our program.
