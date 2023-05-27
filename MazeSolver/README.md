# Prolog Maze Solver

This repository contains a series of Prolog programs designed to solve a pre-defined maze using three different techniques: Follow Left Wall, [Technique 2], and [Technique 3].

## Follow Left Wall

The first technique, **Follow Left Wall**, is implemented in [program1.prolog](link-to-program1.prolog). The maze in this case is represented as a grid where some cells are designated as walls, one cell is the starting point, and another cell is the ending point. The program will explore the maze by trying to move to the left whenever possible. If moving left is not possible, it will try to move forward, and if that is not possible either, it will turn right.

The program uses several helper predicates to define the structure of the maze, to check the existence of a wall, to determine if a cell is a valid move, and to rotate or move in a certain direction. It also includes rules to draw the maze and the path found.

For executing this program, load it into a Prolog interpreter and call the solve predicate:

```prolog
?- solve(Path).
```

This will return the path from the start to the end of the maze, which can be visualized by calling:

```prolog
?- draw(Path).
```

## Best First Search

The second technique, **Best First Search**, is implemented in [program2.prolog](link-to-program2.prolog). This strategy searches the maze by always choosing the cell that is closest to the end, according to the Manhattan distance (which equals the sum of the horizontal and vertical distance from the current cell to the end). The search order is south, east, west, then north.

The maze, starting and ending points, as well as invalid spots are represented in the same way as in the "Follow Left Wall" approach. The new predicates, such as `endNorth`, `endSouth`, `endEast`, `endWest`, `moveNorth`, `moveSouth`, `moveEast`, and `moveWest` are used to check the relative location of the end and to generate a new position in the corresponding direction.

The main predicate `solve` is defined as a recursive function that, at each step, tries to move to a new position and continues from there until it reaches the end.

For executing this program, load it into a Prolog interpreter and call the solve predicate:

```prolog
?- solve(Path).
```

This will return the path from the start to the end of the maze, which can be visualized by calling:

```prolog
?- draw(Path).
```

