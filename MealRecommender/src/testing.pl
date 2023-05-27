length_list(L,X,List):-List=[],L is X.
length_list(L,X,[_|T]):-length_list(L,X+1,T).

drop_last([X|Xs],Ys):-
    drop(Xs,Ys,X).
drop([],[],_).
drop([X1|Xs],[X0|Ys],X0):-
    drop(Xs,Ys,X1).


