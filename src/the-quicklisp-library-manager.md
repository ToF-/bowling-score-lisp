## The QuickLisp library manager

The Common Lisp community offers a lot of libraries. As we will need at least to load a unit testing library, we want to benefit from a good library manager. 

To install [quicklisp](https://www.quicklisp.org/beta/) library manager, 

first download it:
```
curl -o https://beta.quicklisp.org/quicklisp.lisp
```

then install it via a sbcl session:
```
sbcl --load quicklisp.lisp
* (quicklisp-quickstart:install)
```
You can test it by calling the `system-apropos` function:
```
* (ql:system-apropos "regexp")
#<system rte-regexp / regular-type-expression-export-to-quicklisp-502a46e2-git / quicklisp 2022-11-07>
#<system rte-regexp-test / regular-type-expression-export-to-quicklisp-502a46e2-git / quicklisp 2022-11-07>
```
Finally, you can make the library available in all your future sbcl sessions, by having its loading script present in your `.sbclrc` config file.
```lisp
* (ql:add-to-init-file)
(exit)

> cat ~/.sbclrc
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
```
Now you can load and use libraries from the community.
```lisp
> sbcl
* (ql:quickload "cl-ppcre")
To load "cl-ppcre":
  Load 1 ASDF system:
    cl-ppcre
; Loading "cl-ppcre"
..
("cl-ppcre")
* (ppcre:regex-replace "a" "abc" "A")
"Abc"
T
```
