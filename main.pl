:- [tests].

snake(HintsX, HintsY, Input, Output) :- 
    snake_ends(Input, Ends),
    dimentions(Input, Dim),
    random_path(Ends, Dim, Path),
    test_path_hints(Path, HintsX, HintsY),
    draw_path(Path, Input, Output), !.

test_snake(Output) :-
    HintsX = [2, 4, 1, -1],
    HintsY = [3, -1, -1, -1],
    snake(HintsX, HintsY, [
        [-1,  1, -1, -1],
        [-1, -1, -1, -1],
        [-1, -1, -1, -1],
        [-1, -1,  1, -1]
        ], Output).

test_modules() :-
    test_snake_ends(),
    test_draw_path().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finding snake head & tail

snake_ends(Grid, Ends) :-
    Goal = (nth0(X, Grid, Row), nth0(Y, Row, 1)),
    findall((X, Y), Goal, Ends), !.

test_snake_ends() :-
    Grid = [[-1, -1, 1], [-1, 1, -1], [-1, -1, -1]],
    Ends = [(0, 2), (1, 1)],
    snake_ends(Grid, Ends).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Building grid from path

draw_path(Path, Input, Output) :-
    draw_path_aux(Path, (0, 0), Input, [[]], Output), !.    

draw_path_aux(_, _, [], IR1, Output) :-
    maplist(reverse, IR1, IR2),
    reverse(IR2, IR3),
    exclude(=([]), IR3, Output).

draw_path_aux(Path, (_, Y), [[]|Bs], Ds, Output) :-
    NextY is Y + 1,
    draw_path_aux(Path, (0, NextY), Bs, [[]|Ds], Output).

draw_path_aux(Path, (X, Y), [[A|As]|Bs], [Cs|Ds], Output) :-
    ( A == 1 -> C = 1
    ; contains(Path, (Y, X)) -> C = 2
    ; C = 0 ),
    NextX is X + 1,
    draw_path_aux(Path, (NextX, Y), [As|Bs], [[C|Cs]|Ds], Output).

test_draw_path() :-
    Path = [(0, 1)],
    Input = [[1, -1, 1], [-1, -1, -1], [-1, -1, -1]],
    Output = [[1, 2, 1], [0, 0, 0], [0, 0, 0]],
    draw_path(Path, Input, Output).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test that a path matches hints

test_path_hints(Path, HintsX, HintsY) :-
    test_hints(Path, 1, HintsX, 0),
    test_hints(Path, 2, HintsY, 0), !.

test_hints(_, _, [], _).
test_hints(Path, FixArg, [-1|Hints], Level) :-
    NextLevel is Level + 1,
    test_hints(Path, FixArg, Hints, NextLevel), !.

test_hints(Path, FixArg, [Expected|Hints], Level) :-
    fix_count(Path, FixArg, Level, 0, Actual),
    Expected = Actual,
    NextLevel is Level + 1,
    test_hints(Path, FixArg, Hints, NextLevel).

fix_count([], _, _, Count, Count).
fix_count([Cell|Cells], FixArg, FixVal, Count, Output) :-
    arg(FixArg, Cell, FixVal),
    NewCount is Count + 1,
    fix_count(Cells, FixArg, FixVal, NewCount, Output), !.

fix_count([_|Cells], FixArg, FixVal, Count, Output) :-
    fix_count(Cells, FixArg, FixVal, Count, Output).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build a path from head to tail.

random_path([Start, Goal], Dim, Path)
    :- path(Start, Goal, Dim, [Start], Path).

path(Goal, Goal, _, Path, Path).
path(Start, Goal, Dim, Path, Output) :- 
    move2d(Start, Move, Dim),
    unique(Move, Path),
    path(Move, Goal, Dim, [Move|Path], Output).

% Move diagonally
move2d((X1, Y1), (X2, Y2), (MaxX, MaxY)) :-
    move(X1, X2, MaxX),
    move(Y1, Y2, MaxY).

% Move horizontally / vertically
move2d((X1, Y), (X2, Y), (MaxX, _)) :- move(X1, X2, MaxX).
move2d((X, Y1), (X, Y2), (_, MaxY)) :- move(Y1, Y2, MaxY).

% Move back / forward
move(X1, X2, Max) :- X2 is X1 + 1, X2 < Max.
move(X1, X2, _)   :- X2 is X1 - 1, X2 >= 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities

contains([], _) :- fail.
contains([X|Xs], Y) :- X == Y; contains(Xs, Y).

len(Xs, Len) :- len_aux(Xs, 0, Len).
len_aux([], Len, Len).
len_aux([_|Xs], Len, Output) :-
    NextLen is Len + 1,
    len_aux(Xs, NextLen, Output).

dimentions([], (0, 0)).
dimentions([X | Xs], (LenX, LenY)) :- 
    len([X|Xs], LenX),
    len(X, LenY), !.

unique(_, []).
unique(X, [Y|Ys]) :- X \= Y, unique(X, Ys).

first((X, _), X).
second((_, X), X).