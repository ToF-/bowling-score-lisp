# The Bowling Score Kata

The main part of our program will be a function that given an input list of rolls computes the score for these rolls.

The input list contains no indication of which frame a roll is part of, so we have to keep track of the frame number. 

On the tenth frame, a strike on the first roll will grant two more rolls, but a strike on these rolls will not generate new bonus.

Thus, the game 

`10  10  10  0 0  0 0  0 0  0 0  0 0  0 0  0 0` 

will yield a score of 60, but the game

`0 0  0 0  0 0  0 0  0 0  0 0  0 0  0 0  0 0  10 10 10` 

will yield a score of only 30.

The way to keep track of frames is to look for tens : 
    - a 10 at the beginning of the list means a strike, and the second frame starts with the second roll is the list
    - a 10 at the beginning of a frame also means that the next roll is the first of a new frame, except if we are already at frame 10
    - otherwise a frame is composed of 2 rolls

Thus in order to correctly compute the score, our function will have to keep track of 
    - the frame number
    - the remaining rolls to count from the list
    - the accumulating score

The TDD method consists in creating capacities for the program one step at a time, following a cycle of 3 steps: 
- writing a failing test
- making the test pass
- refactoring

Our todo list for the Bowling Score Kata starts with simple cases and progressively adds features :
- averages games (no bonus points)
- strike roll on the first frame
- strike rolls on any frame
- spare on the first frame
- spares on any frames
- tenth frame rule (only bonus rolls are counted)
