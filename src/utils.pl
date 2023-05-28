
decr_at(Idx, List, NewList, NewVal) :-
    length(Before, Idx),
    append(Before, [Val|After], List),
    NewVal is Val - 1,
    append(Before, [NewVal|After], NewList).

% Utils with grids

row(X, Grid, Row) :-
    nth0(X, Grid, Row), !.

column(_, [], []) :- !.
column(Idx, [X|Xs], [Y|Ys]) :-
    nth0(Idx, X, Y),
    column(Idx, Xs, Ys).

len2d([], (0, 0)).
len2d([X | Xs], (LenX, LenY)) :- 
    length([X|Xs], LenX),
    length(X, LenY).

map2d(_, _, _, [], []).
map2d((_, Y), Fun, Input, [[]|Bs], [[]|Ds]) :-
    NextY is Y + 1,
    map2d((0, NextY), Fun, Input, Bs, Ds), !.

map2d((X, Y), Fun, Input, [[A|As]|Bs], [[C|Cs]|Ds]) :-
    call(Fun, (X, Y), Input, A, C),
    NextX is X + 1,
    map2d((NextX, Y), Fun, Input, [As|Bs], [Cs|Ds]).

find2d(Grid, Tag, (X, Y)) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Tag).

find2d_all(Grid, Tag, Cells) :-    
    Xs = nth0(X, Grid, Row),
    Ys = nth0(Y, Row, Tag),
    findall((X, Y), (Xs, Ys), Cells).

print3d(Grid) :- maplist(print3d_row, Grid).
print3d_row(Row) :- maplist(print3d_item, Row), nl.
print3d_item(Item) :- write(Item), write(' ').

print2d(Grid) :- maplist(print2d_row, Grid).
print2d_row(Row) :-
    maplist(format_item, Row, Rowf),
    atomic_list_concat(Rowf, ' ', Atom),
    writeln(Atom).

format_item(Item, Itemf) :-
    format(atom(Itemf), '~` t~w~5+', [Item]).

% Utils with tuples

count_tuples([], _, _, 0).
count_tuples([Tuple|Tuples], FixArg, FixVal, Count) :-
    count_tuples(Tuples, FixArg, FixVal, Left),
    ( arg(FixArg, Tuple, FixVal) 
    -> Count is Left + 1; Count = Left ).

seconds([], []).
seconds([(_,A)|Tuples], [A|As]) :-
    seconds(Tuples, As).