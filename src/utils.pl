decrement_at(Idx, List, NewList, Val) :-
    length(Before, Idx),
    append(Before, [X|After], List),
    Val is X - 1,
    append(Before, [Val|After], NewList).

find2d((X, Y), Grid, Tag) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Tag).

find_cells(Grid, Tag, Moves) :-    
    Xs = nth0(X, Grid, Row),
    Ys = nth0(Y, Row, Tag),
    findall((X, Y), (Xs, Ys), Moves).

dimentions([], (0, 0)).
dimentions([X | Xs], (LenX, LenY)) :- 
    length([X|Xs], LenX),
    length(X, LenY).

unique(_, []).
unique(X, [Y|Ys]) :- X \= Y, unique(X, Ys).

row(X, Grid, Row) :-
    nth0(X, Grid, Row), !.

column(_, [], []) :- !.
column(Idx, [X|Xs], [Y|Ys]) :-
    nth0(Idx, X, Y),
    column(Idx, Xs, Ys).

seconds([], []).
seconds([(_,A)|Tuples], [A|As]) :-
    seconds(Tuples, As).

map2d(_, _, _, [], []).
map2d((_, Y), Pred, Input, [[]|Bs], [[]|Ds]) :-
    NextY is Y + 1,
    map2d((0, NextY), Pred, Input, Bs, Ds), !.

map2d((X, Y), Pred, Input, [[A|As]|Bs], [[C|Cs]|Ds]) :-
    call(Pred, (X, Y), Input, A, C),
    NextX is X + 1,
    map2d((NextX, Y), Pred, Input, [As|Bs], [Cs|Ds]).