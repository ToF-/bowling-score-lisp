## Creating Lists

As we have seen, the `list` function allow us to define lists, provided we have the values:
```lisp
* (list 'foo 'bar 42 17)
(FOO BAR 42 17)
```
How do we create a list, one element at a time ? By using `cons`. 
```lisp
* (cons 'a '(b c d))
(A B C D)
```
But how do you _start_ a list ? You start by `cons`ing an element to `NIL`.
```lisp
* (cons 'start NIL)
(START)
```

Here's a more powerful example. Let's say we want to build the list formed by the numbers from _n_ to _m_. We are to define a recursive function. 
- it's term is when the argument _n_ is greater than _m_: then the list is complete, and me must stop.
- otherwise, we do two things:
  - form a list of numbers from _n + 1_ to _m_, which is made by calling recurively our function,
  - `cons` _n_ to that list

```lisp
* (defun seq (n m)
(if (> n m) nil (cons n (seq (1+ n) m))))
SEQ
* (seq 0 9)
(0 1 2 3 4 5 6 7 8 9)
*
```



