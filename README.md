# A Racket tree

Simple program that draws a recursive tree. A tree of depth 1 is a V shape. This was written as a means to pick up the Racket language again. 

## st-decay-spread-rotate
The main function that computes the tree. A tree is a list of lines. A line is a struct consisting of points and a point is a struct with an x and y coordinate.

Parameters:
- depth: tree depth
- seed: a point where the tree starts
- alpha: the angle between the two branches of a tree
- len: the length of the two tree branches at depth 1
- decay: value that is multiplied with len to create a new len at the next depth (deeper branches are shorted/longer)
- spread: value that is multiplied with alpha to create a new alpha at the next depth 
- rot: radians the V of a tree is rotated at every level this happens symmetrically
- trot: this should be 0 :)

## draw-tree
A function to write a PNG file with the drawn tree.

## The GUI
If the file is run, it opens a Racket GUI to play with the tree parameters.

Have fun.
