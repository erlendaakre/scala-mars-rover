# codingtest-itv-mars-rover

## Guidance
Our aim is to get a sense of how you approach a programming problem.
We would like you to spend roughly two hours on the following problem. This time restriction
is not to see how quickly you can work, it’s an attempt to be fair to candidates with outside
work commitments.
Similarly the challenge has been broken down into three stages. You are encouraged to start
with Part 1 and work your way through. It is more important that the code you submit is in
good order and representative of your working practices and abilities, than for all parts of the
challenge to have been completed.
Your solution should be a standard SBT project that compiles and shows an understanding
of testing practices.
Your solution to this challenge may form part of the next stage of your interview.

## Brief
The next Mars Rover is being developed, and we need you to come up with a simple way of
issuing navigation instructions to it from back on Earth!
We would like you to model the following.

### Part 1: Basic Movement
1. The Mars Rover operates on a grid of arbitrary size.
2. You can only issue three commands: Move forward, rotate clockwise, and rotate
anticlockwise.
3. If the rover moves off the grid, it reappears on the opposite side of the grid.
4. 
### Part 2: Autopilot
1. Devise a simple process for determining the shortest possible path from one position
on the grid to another.
2. Improve the solution so that it can avoid mountain ranges that occupy a number of
inconvenient grid squares scattered around the map.

### Part 3: Putting it all together
Output all the instructions and moves carried out by the rover to get from one grid square to
another. The output can take any form e.g rows of text, JSON data, or something graphical.
