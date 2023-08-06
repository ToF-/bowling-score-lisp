# The Bowling Score Kata

Our task will be to write a program which, given a series of rolls delivered by a Ten Pin Bowling player, computes the current score of this player. The roll values will be consistent with the game rules: no illegal values (such as -1, 11 or values totaling more than 10 in a frame). In test cases where not all rolls have been played, the resulting value should be the minimum score obtained (i.e the score value if all the subsequent rolls were 0).

Our program's task will consist in reading the standard input stream, parsing the given numbers, computing and printing the scores. 

## Input specification

- _T_ : the number of test cases, then _T_ test cases follows.
- each test case consists in 2 lines:
  - _N_ : the number of rolls delivered, ( 0 < N ≤ 21 )
  - _R1,..Rn_ - the rolls delivered ( 0 ≤ R ≤ 10 )

## Output specification
For each test case output one integer: the score made by the player after they played all the rolls in the test case.


## Example
### Input

    3
    2
    4 6
    4
    10 7 3 5
    12
    10 10 10 10 10 10 10 10 10 10 10 10

### Output

    10
    40
    300
