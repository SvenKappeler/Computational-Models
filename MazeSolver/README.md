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

