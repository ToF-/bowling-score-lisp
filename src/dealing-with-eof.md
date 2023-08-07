## Dealing with EOF

 The test on empty source fails. Let's reproduce this failure via the repl:
 ```lisp
 > rlwrap sbcl --load bowling.lisp
 * (with-input-from-string (s "") (read-numbers s))

debugger invoked on a END-OF-FILE in thread
#<THREAD "main thread" RUNNING {1004498113}>:
  end of file on #<dynamic-extent STRING-INPUT-STREAM (unavailable) from "">

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

((FLET SB-IMPL::CHARACTER-IN :IN SB-IMPL::%INIT-STRING-INPUT-STREAM) #<SB-IMPL::STRING-INPUT-STREAM {6EA7863}> T 0)
```
The "end of file" situation on our stream is an error condition that interrupts our program. 
This can be fixed with one of the (optional) parameters of `read`.

> Function **READ**\
> - Syntax:\
>   - **read** *&optional input-stream eof-error-p eof-value recursive-p* => *object*\
> - Arguments and Values:\
>   - *input-stream* — an input stream designator.\
>   - *eof-error-p* — a generalized boolean. The default is true.\
>   - *eof-value* — an object. The default is nil.\
>   - *recursive-p* — a generalized boolean. The default is false.\
>   - *object* — an object (parsed by the Lisp reader) or the eof-value.\
> …
>
> If a file ends in a symbol or a number immediately followed by an end of file, **read** reads the symbol or number successfully; when called again, it sees the end of file and only then acts according to *eof-error-p*.

We have to indicate that the end of file shouldn't be an error. Let's use a constant to document this.
```lisp
(defconstant NO-EOF-ERROR nil)

(defun read-numbers (source)
  (let ((n (read source NO-EOF-ERROR)))
    (if (null n) nil
      (cons n nil))))
```
Now all the tests pass.
