unit: tests.lisp bowling.lisp
	sbcl --noinform --load tests.lisp

acceptance: bowling.lisp sample.txt
	./score sample.txt >result.txt
	diff expected.txt result.txt
