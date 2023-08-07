## Processing the Games in Input

As defined in the [introduction](./the-program.md), our program's task is to:

- read the input stream, taking all the numbers from there
- interpret this list of numbers as _information_ about the test cases:
    - a number _T_ of tests cases followed by _T_ series of
        - a number _N_ of rolls in this test case, followed by
        - _R1_ _R2_ … _RN_ numbers: the rolls themselves for this test case
- produce the scores for these games,
- print each of these scores, separated by a new line.

In this chapter, we will interest ourselves in the main function of our program. It will consume a list, and return a list.

For instance, processing the list `(3 2 4 0 4 3 4 1 8 5 0 1 7 1 8)` should result the list `(4 16 17)`. Why?

- the first number indicates 3 games to process. The data for these 3 games is formed by the list `(2 4 0 4 3 4 1 8 5 0 1 7 1 8)`.
- the first number the first game has 2 rolls: `(4 2)` for which computing the score will yield `4`. The rest to process is the list `(4 3 4 1 8 5 0 1 7 1 8)`.
- the second game has 4 rolls: `(3 4 1 8)` for which computing the score will yield `16`. The rest to process it the list `(5 0 1 7 1 8)`
- the third game has 5 rolls: `(0 1 7 1 8)` for a score of `17` and the rest to process is the empty list, which will end the process.

We are going to write this function using TDD.
