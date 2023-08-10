# Final Assembly
We are almost done: the last step consists in assembling our functions in the main program, `score.lisp` ;
```
; score.lisp
(load "bowling")

(format-numbers T 
  (mapcar #'score 
          (extract-games 
                (cdr (read-numbers *standard-input*)))))
```

Now we can run the acceptance test:
```
> make accept
sbcl --script score.lisp <test-cases.txt >results.txt
diff expected.txt results.txt
```

No diff: the test passes. 

We can also use the program interactively:
```
>sbcl --script score.lisp
1 ↵
5 ↵
10 3 4 2 0 ↵ <ctl-D>
26
```





