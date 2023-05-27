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

%InvalidSpots
badSpot([X,Y]) :- wall([X,Y]).
badSpot([X,Y]) :- X < 1; Y < 1.
badSpot([X,Y]) :- mazeSize([W, H]), (X > W; Y > H).

%The End it to the _ Direction
endNorth([CurrX,CurrY]) :- TestY is CurrY, end([X,Y]), TestY > Y.
endSouth([CurrX,CurrY]) :- TestY is CurrY, end([X,Y]), TestY < Y.
endEast([CurrX,CurrY]) :- TestX is CurrX, end([X,Y]), TestX < X.
endWest([CurrX,CurrY]) :- TestX is CurrX, end([X,Y]), TestX > X.


%Move North, Last South
moveNorth([CurrX, CurrY], [CurrX, NextY]) :- NextY is CurrY - 1.
moveNorth([CurrX, CurrY], [NextX, NextY]) :- NextY = CurrY, NextX is CurrX - 1.
moveNorth([CurrX, CurrY], [NextX, NextY]) :- NextY = CurrY, NextX is CurrX + 1.
moveNorth([CurrX, CurrY], [NextX, NextY]) :- NextY is CurrY + 1, NextX = CurrX.

%Move South, Last North
moveSouth([CurrX, CurrY], [NextX, NextY]) :- NextY is CurrY + 1, NextX = CurrX.
moveSouth([CurrX, CurrY], [NextX, NextY]) :- NextY = CurrY, NextX is CurrX - 1.
moveSouth([CurrX, CurrY], [NextX, NextY]) :- NextY = CurrY, NextX is CurrX + 1.
moveSouth([CurrX, CurrY], [CurrX, NextY]) :- NextY is CurrY - 1.

%Move East, Last West
moveEast([CurrX, CurrY], [NextX, NextY]) :- NextY = CurrY, NextX is CurrX + 1.
moveEast([CurrX, CurrY], [CurrX, NextY]) :- NextY is CurrY - 1.
moveEast([CurrX, CurrY], [NextX, NextY]) :- NextY is CurrY + 1, NextX = CurrX.
moveEast([CurrX, CurrY], [NextX, NextY]) :- NextY = CurrY, NextX is CurrX - 1.


%Move West, Last East
moveWest([CurrX, CurrY], [NextX, NextY]) :- NextY = CurrY, NextX is CurrX - 1.
moveWest([CurrX, CurrY], [CurrX, NextY]) :- NextY is CurrY - 1.
moveWest([CurrX, CurrY], [NextX, NextY]) :- NextY = CurrY, NextX is CurrX + 1.
moveWest([CurrX, CurrY], [NextX, NextY]) :- NextY is CurrY + 1, NextX = CurrX.

%Try to Move _
try(CurrentLocation, NewLocation) :-
    endSouth(CurrentLocation),
	moveSouth(CurrentLocation, NewLocation).
try(CurrentLocation, NewLocation) :-
    endEast(CurrentLocation),
	moveEast(CurrentLocation, NewLocation).
try(CurrentLocation, NewLocation) :-
    endWest(CurrentLocation),
	moveWest(CurrentLocation, NewLocation).
try(CurrentLocation, NewLocation) :-
    endNorth(CurrentLocation),
	moveNorth(CurrentLocation, NewLocation).


% A solution of the maze from our current location is [] if our current location is the end.
solve(CurrentLocation, Path) :- end(CurrentLocation), Path = [CurrentLocation].


solve(CurrentLocation, Path) :-
		try(CurrentLocation, NewLocation),
    	\+badSpot(NewLocation),
        solve(NewLocation, RestOfPath),
        Path = [CurrentLocation | RestOfPath].

% Convenience rule
solve(Path) :- start(Start), solve(Start, Path).


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
