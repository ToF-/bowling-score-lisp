# Setting up a common lisp environment

We will use [SBCL](http://www.sbcl.org/). 

## installing `sbcl`

on macos:
```
brew install sbcl
```

on ubuntu:
```
apt install sbcl
```

## wrapping sbcl sessions with `rlwrap`

as `sbcl` doesn't allow command editing and navigation in command history, we will wrap our sbcl sessions with `rlwrap sbcl` :
```lisp
* (+ 42 17)
59
* (format t "hello world!")
hello world!
nil
* (exit)
```
we will use the [quicklisp](https://www.quicklisp.org/beta/) library manager :

downloading:
```
curl -o https://beta.quicklisp.org/quicklisp.lisp
```

to install the library manager, `sbcl --load quicklisp.lisp` then:
```lisp
; installing the manager
* (quicklisp-quickstart:install)

; testing it
* (ql:system-apropos "regexp")
#<system rte-regexp / regular-type-expression-export-to-quicklisp-502a46e2-git / quicklisp 2022-11-07>
#<system rte-regexp-test / regular-type-expression-export-to-quicklisp-502a46e2-git / quicklisp 2022-11-07>

; making it available in every sbcl session
(ql:add-to-init-file)
(exit)
```
