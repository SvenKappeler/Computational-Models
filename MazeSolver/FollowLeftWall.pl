% Walls
wall([X,1]) :- X =\= 2.
wall([X,2]) :- X = 1; X = 4; X = 6.
wall([X,3]) :- X = 1; X = 2; X = 6.
wall([X,4]) :- X =\= 3,X =\= 5.
wall([X,5]) :- X = 1; X = 4; X = 6.
wall([X,6]) :- X =\= 2.

% Start / end
start([2,1]).
end([2,6]).

% Maze Size
mazeSize([6,6]).

%Invalid NewLocations
badSpot([X,Y]) :- wall([X,Y]).
badSpot([X,Y]) :- X < 1; Y < 1.
badSpot([X,Y]) :- mazeSize([W, H]), (X > W; Y > H).

% North == 0
% East == 1
% South == 2
% West == 3


%CurrentLocation Has a Left Wall
hasLeftWall([X, Y], Direction) :- Direction = 0, TestX is X - 1, wall([TestX,Y]).
hasLeftWall([X, Y], Direction) :- Direction = 1, TestY is Y - 1, wall([X,TestY]).
hasLeftWall([X, Y], Direction) :- Direction = 2, TestX is X + 1, wall([TestX,Y]).
hasLeftWall([X, Y], Direction) :- Direction = 3, TestY is Y + 1, wall([X,TestY]).

%CurrentLocation Has a Foward Wall
hasFowardWall([X,Y], Direction) :- Direction = 0, TestY is Y - 1, wall([X,TestY]).
hasFowardWall([X,Y], Direction) :- Direction = 1, TestX is X + 1, wall([TestX,Y]).
hasFowardWall([X,Y], Direction) :- Direction = 2, TestY is Y + 1, wall([X,TestY]).
hasFowardWall([X,Y], Direction) :- Direction = 3, TestX is X - 1, wall([TestX,Y]).

%Rotate Direction 90 Clockwise
rotateClockwise(CurrDirect, NewDirect) :- CurrDirect = 0, NewDirect is CurrDirect + 1.
rotateClockwise(CurrDirect, NewDirect) :- CurrDirect = 1, NewDirect is CurrDirect + 1.
rotateClockwise(CurrDirect, NewDirect) :- CurrDirect = 2, NewDirect is CurrDirect + 1.
rotateClockwise(CurrDirect, NewDirect) :- CurrDirect = 3, NewDirect is CurrDirect - 3.

%Rotate Direction 90 Counter Clockwise
rotateCounterClockwise(CurrDirect, NewDirect) :- CurrDirect = 0, NewDirect is CurrDirect + 3.
rotateCounterClockwise(CurrDirect, NewDirect) :- CurrDirect = 1, NewDirect is CurrDirect - 1.
rotateCounterClockwise(CurrDirect, NewDirect) :- CurrDirect = 2, NewDirect is CurrDirect - 1.
rotateCounterClockwise(CurrDirect, NewDirect) :- CurrDirect = 3, NewDirect is CurrDirect - 1.

%Move to NewLocation
move([CurrX, CurrY], [CurrX, NextY], Direction) :- Direction = 0,
    NextY is CurrY - 1.
move([CurrX, CurrY], [NextX, NextY], Direction) :- Direction = 1,
    NextX is CurrX + 1, NextY = CurrY.
move([CurrX, CurrY], [NextX, NextY], Direction) :- Direction = 2,
    NextX = CurrX, NextY is CurrY + 1.
move([CurrX, CurrY], [NextX, NextY], Direction) :- Direction = 3,
    NextY = CurrY, NextX is CurrX - 1.
moveNowhere([CurrX, CurrY], [NextX, NextY]) :- NextX = CurrX, NextY = CurrY.

%Movement IF There is NO Left Wall
try(CurrentLocation, CurrentDirection, NewLocation, NewDirection) :-
    \+ hasLeftWall(CurrentLocation, CurrentDirection),
    rotateCounterClockwise(CurrentDirection, NewDirection),
    move(CurrentLocation, NewLocation, NewDirection).

%Movement IF There is A Left Wall BUT No Foward Wall
try(CurrentLocation, CurrentDirection, NewLocation, NewDirection) :-
	hasLeftWall(CurrentLocation, CurrentDirection),
    \+hasFowardWall(CurrentLocation, CurrentDirection),
    move(CurrentLocation, NewLocation, CurrentDirection),
    NewDirection is CurrentDirection.

%Movement IF There is A Left Wall AND Foward Wall
try(CurrentLocation, CurrentDirection, NewLocation, NewDirection) :-
	hasLeftWall(CurrentLocation, CurrentDirection),
    hasFowardWall(CurrentLocation, CurrentDirection),
    rotateClockwise(CurrentDirection, NewDirection),
	moveNowhere(CurrentLocation, NewLocation).

% A solution of the maze from our current location is [] if our current location is the end.
solve(CurrentLocation, _, Path) :- end(CurrentLocation), Path = [CurrentLocation].


solve(CurrentLocation, CurrentDirection, Path) :-
    	try(CurrentLocation, CurrentDirection, NewLocation, NewDirection),
        \+badSpot(NewLocation),
        solve(NewLocation, NewDirection, RestOfPath),
        Path = [CurrentLocation | RestOfPath].

% Convenience rule (Initally Direction is South, However it will still Work on Any Maze, Just Needs a Number Here)
solve(Path) :- start(Start), solve(Start, 2, Path).


drawCell(Column, Row, _) :- wall([Column, Row]), write("X"), !.
drawCell(Column, Row, _) :- start([Column, Row]), write("S"), !.
drawCell(Column, Row, _) :- end([Column, Row]), write("E"), !.
drawCell(Column, Row, Path) :- member([Column, Row], Path), write("P"), !.
drawCell(_, _, _) :- write("O").

drawRow(Row, Path) :- drawCell(1, Row, Path), tab(1), drawCell(2, Row, Path), tab(1),
        drawCell(3, Row, Path), tab(1), drawCell(4, Row, Path), tab(1),
        drawCell(5, Row, Path), tab(1), drawCell(6, Row, Path), nl.

draw :- drawRow(1, []), drawRow(2, []), drawRow(3, []), drawRow(4, []), drawRow(5, []),
        drawRow(6, []).
draw(Path) :- drawRow(1, Path), drawRow(2, Path), drawRow(3, Path), drawRow(4, Path),
        drawRow(5, Path), drawRow(6, Path).
