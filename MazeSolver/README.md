# Prolog Maze Solver

This repository contains a series of Prolog programs designed to solve a pre-defined maze using three different techniques: Follow Left Wall, Manhattan Distance, and Heuristic Wall Following with Manhattan Distance.

## Follow Left Wall

The first technique, **Follow Left Wall**, is implemented in [FollowLeft.prolog](https://github.com/SvenKappeler/Computational-Models/blob/main/MazeSolver/FollowLeftWall.pl). The maze in this case is represented as a grid where some cells are designated as walls, one cell is the starting point, and another cell is the ending point. The program will explore the maze by trying to move to the left whenever possible. If moving left is not possible, it will try to move forward, and if that is not possible either, it will turn right.

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

The second technique, **Best First Search**, is implemented in [PreferEnd.prolog](https://github.com/SvenKappeler/Computational-Models/blob/main/MazeSolver/PreferEndDirection.pl). This strategy searches the maze by always choosing the cell that is closest to the end, according to the Manhattan distance (which equals the sum of the horizontal and vertical distance from the current cell to the end). The search order is south, east, west, then north.

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

## Heuristic Wall Following with Manhattan Distance

The third technique, **Heuristic Wall Following with Manhattan Distance**, is implemented in [FLW+PEN.prolog](https://github.com/SvenKappeler/Computational-Models/blob/main/MazeSolver/FLW%2BPEN.pl). This approach combines the best parts of the two previous methods, offering a more efficient solution.

The algorithm checks if there's a branching path it can take, and then decides the direction based on the proximity of the wall in front of it to the end of the maze. If the wall in front is closer to the end, it chooses that path. This can efficiently prevent the algorithm from walking into dead-end hallways and shortens the overall path length.

To identify whether the forward wall is closer to the end, the `fowardWallCloser` predicate is used. It calculates the Manhattan distance to the end before and after the potential move and compares these values.

The `hasLeftWall` and `hasFowardWall` predicates are used to check the presence of walls in the left and forward directions. Based on these checks and the `fowardWallCloser` outcome, the `try` predicate makes a decision: move forward, rotate left, rotate right, or stay in place.

The `solve` predicate is similar to the one used in previous techniques, but with an added parameter to account for the current direction.

To execute this program, load it into a Prolog interpreter and call the solve predicate:

```prolog
?- solve(Path).
```

This will return the path from the start to the end of the maze, which can be visualized by calling:

```prolog
?- draw(Path).
```
