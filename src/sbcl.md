# SBCL
[SBCL](https://www.sbcl.org/) is the most popular implementation of common lisp, and it generates machine code that is fast (not that it matters much for our kata!).

To quote [Wikipedia](https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp):

> Steel Bank Common Lisp (SBCL) is a free Common Lisp implementation that features a high-performance native compiler, Unicode support and threading.
>
> The name "Steel Bank Common Lisp" is a reference to Carnegie Mellon University Common Lisp from which SBCL forked: Andrew Carnegie made his fortune in the steel industry and Andrew Mellon was a successful banker.

## installing sbcl

On a Mac with macos:
```
> brew install sbcl
```

On a linux machine with ubuntu:
```
> sudo apt install sbcl
```

To chech your installation:
```
> sbcl --version
SBCL 2.3.4
```

## wrapping interactive sessions with `rlwrap`

As `sbcl` doesn't allow command editing and navigation in command history, we will wrap our sbcl sessions with the command: `rlwrap sbcl`.

On a Mac with macos:
```
> brew install rlwrap
```

On a linux machine with ubuntu:
```
> sudo apt install rlwrap
```

## the Read-Eval-Print Loop
## executing lisp scripts
## writing unit tests with  `lisp-unit`

