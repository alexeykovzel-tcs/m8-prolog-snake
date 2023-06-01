
% Decrement a value at a given index in the array 
decr_at(Idx, List, NewList, NewVal) :-
    length(Before, Idx),
    append(Before, [Val|After], List),
    NewVal is Val - 1,
    append(Before, [NewVal|After], NewList).

% Calculate dimentions of a grid
len2d([], (0, 0)).
len2d([X|Xs], (LenX, LenY)) :- 
    length([X|Xs], LenX),
    length(X, LenY).

% Convert a grid, by applying a function to each cell
map2d(_, _, _, [], []).
map2d((_, Y), Fun, Input, [[]|Bs], [[]|Ds]) :-
    NextY is Y + 1,
    map2d((0, NextY), Fun, Input, Bs, Ds), !.

map2d((X, Y), Fun, Input, [[A|As]|Bs], [[C|Cs]|Ds]) :-
    call(Fun, (X, Y), Input, A, C),
    NextX is X + 1,
    map2d((NextX, Y), Fun, Input, [As|Bs], [Cs|Ds]).

% Find a value in the grid
find2d(Grid, Value, (X, Y)) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Value).

% Find all cells that contain a certain value 
find2d_all(Grid, Value, Cells) :-    
    Xs = nth0(X, Grid, Row),
    Ys = nth0(Y, Row, Value),
    findall((X, Y), (Xs, Ys), Cells).

% Count elements by a given argument in tuples 
count_tuples([], _, _, 0).
count_tuples([Tuple|Tuples], FixArg, FixVal, Count) :-
    count_tuples(Tuples, FixArg, FixVal, Left),
    ( arg(FixArg, Tuple, FixVal) 
    -> Count is Left + 1; Count = Left ).

% Get second elements from an array of tuples
seconds([], []).
seconds([(_,A)|Tuples], [A|As]) :-
    seconds(Tuples, As).