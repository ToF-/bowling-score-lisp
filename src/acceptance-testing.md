## Acceptance testing

Our acceptance testing strategy will be straightforward:
- define a standard input acceptance test file: `test-cases.txt`
- define a reference result file: `expected.txt`
- execute the program, feeding it the acceptance test file, and capturing the results in `results.txt`
- compare `results.txt` with `expected.txt`

Let's create an empty `score.lisp` program for now:
```
> touch score.lisp
```
Then a `score` terminal script:
```
#!/bin/bash
sbcl --script score.lisp
```
This is our test cases file: `test-cases.txt`

```
5
4
3 5 2 7
6
10 5 4 10 5 2
12
10 10 10 10 10 10 10 10 10 10 10 10
20
3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5
3
10 10 10
```
And this is the expected result file: `expected.txt`
```
17
52
300
80
30
```

To run the test, we launch the (currently empty) script and then make the comparison:
```
 ./score <test-cases.txt  >results.txt && diff expected.txt results.txt
1,5d0
< 17
< 52
< 300
< 80
< 60
```
which of course results in a difference, meaning a failed a test.
