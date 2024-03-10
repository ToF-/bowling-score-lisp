# The Bowling Score Kata

Let's get back at the Bowling Score Kata. Most probably the core of our program will be a function that, given a list of rolls, returns the score value.

Several obstacles present themselves :

- the bonus rule is different on the last frame of the game :
    - when playing frames 1 to 9, a spare or a strike causes following rolls in the input list to be added as extra points in the frame score
    - when playing frame 10, rolling a spare or a strike doesn't create additional extra points, the input is already filled with the extra roll or rolls

- the input doesn't contain an indication of which frame we are currently counting, this information depends on the content of a given element in the list :
    - if we are at the beginning of a frame, and the roll is 10, the next frame will start on the next roll in the list
    - otherwise the next frame will start 2 rolls from our position in the list

Thus in order to correctly compute the score, our function will have to keep track of 
    - the accumulating score
    - where the next frame is starting in the list of roll, or what are the remaining rolls to count
    - are we on frame 10 already 

The TDD method consists in creating capacities for the program one step at a time, following a cycle of 
- writing a failing test
- make the pass
- refactor

Our todo list for the Bowling Score Kata starts with simple cases and progressively adds features :
- averages games (no bonus points)
- strike roll on the first frame
- strike rolls on any frame
- spare on the first frame
- spares on any frames
- tenth frame rule (only bonus rolls are counted)
