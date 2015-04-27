# Tune-Traveler
Tune Traveler is the final project for the Spring 2015 Organization of Programming Languages class.
The idea behind the project was to implement the A* path-finding algorithm visually and then, as a
little something extra, give it some sound. We wanted the algorithm to generate a "song" as it traverses
the nodes in the grid by playing a random sound every time it moves from node to node.

Authors: Darin Beaudreau, Xiaoyang Li, ChoChak Wong

## What is A*?
The A* path-finding algorithm is widely used in video game development as a means of finding the shortest path from point A to B -- that is, it is used to find a path that can be traveled to reach the destination in the least amount of movement. Chances are, if you've played a game where an object in a game found its way through a maze, moved to a position in a certain way, or anything like that, it was the A* algorithm being used.

## How does it work? (thinky words ahead)
A* is a slightly fancier version of Djikstra's Algorithm, another path-finding algorithm taught in Discrete Structures. The difference between the two being that A* uses a heuristic search, where Djikstra's lacks this component.

What it all boils down to is F = G + H. Each node in the grid is assigned an F score, which is the "cost" of moving to that node. The G score is calculated by simply determining whether moving to that node would require a horizontal/vertical movement, or a diagonal movement. If it is horizontal or vertical, a value of 10 is assigned (or 1, doesn't really matter, as long as everything is consistent). If the movement is diagonal, the value 14 is assigned. This is because moving from one node to a node to the left or right, for example, only requires moving 1 space. The distance between the current node and a node that is diagonal from it would require a number of spaces of movement equal to the square root of 2. In decimal, that is 1.4141414... so both numbers are multiplied by 10 to get 10 and 14.

The H score represents the vertical and horizontal offset from the goal multiplied by 10. If you remember rise over run from algebra, you'll understand. The H score is the sum of these two offsets multiplied by 10.

Adding these two scores together gives you the "cost" of moving to that node. Then, the algorithm simply chooses the path of which has the least F cost. So the path with the least sum of F scores is the path it takes.

## What does it look like in action?
I'm glad you asked that, because what is a project demonstration without pretty pictures to hold the reader's attention? Here are a few "mazes" that have been solved by the algorithm.

(put pictures here)

## How can I play with it?
Simply fork/clone this repository, or download the ZIP, to get the files. Then, open "tune-traveler.rkt" in DrRacket. Assuming you have the RSound library installed, you're good to go. Just hit Run and watch!

## Can I make my own levels?
You sure can. Just create a text file with 15 rows of 15 characters. If you don't know what I mean by this, look at the levels that come with this repository. It is a good idea to put a border around the maze, though it is not required. Here is a key of the characters used.

1 - Wall

0 - Blank/Nothing

S - Starting position.

E - End position.

Then go to "levels.rkt" and add the level to the "maps" list in the same way that the others were added and in "tune-traveler.rkt", change the "LEVEL" variable to the list index for your level (if your level is the 5th in the list, change it to 4).

And that should do it!
You can also edit the "GRID_SIZE" variable in "constants.rkt" if you want to make larger levels. Just make sure you don't try to load a level of a different size.
