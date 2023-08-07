## Reading From a File

Of course, if we can read values from the standard input as a source, it must be possible to read from another source ?

Of course! Let's try it:
```lisp
> echo "4807" >value.txt
> rlwrap sbcl
* (defvar *my-stream* (open "./value.txt"))
MY-STREAM
* (read *my-stream*)
4807
```

Can we read _several_ values, and put them in a list ? Again, let's try.
```lisp
> echo "42" >values.txt
> echo "17" >>values.txt
> echo "23" >>values.txt
> cat values.txt
42
17
23
> rlwrap sbcl
* (defvar *s* (open "./values.txt"))
*S*
* (let ((v1 (read *s*)) (v2 (read *s*)) (v3 (read *s*)))
    (cons v1 (cons v2 (cons v3 nil))))
(42 17 23)
```

