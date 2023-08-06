unit: tests.lisp bowling.lisp
	sbcl --script tests.lisp

acceptance: bowling.lisp test-cases.txt expected.txt score.lisp score
	./score <test-cases.txt >results.txt
	diff expected.txt results.txt
