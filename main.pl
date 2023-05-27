:- [tests].

snake(HintsX, HintsY, Input, Output) :- 
    Hints = (HintsX, HintsY),
    snake_ends(Input, Ends),
    dimentions(Input, Dim),
    random_path(Ends, Dim, Hints, Path),
    test_path(Path, Hints),
    draw_path(Path, Input, Output), !.

test_snake(Output) :-
    HintsX = [-1, -1, -1, -1],
    HintsY = [-1,  1,  3,  3],
    snake(HintsX, HintsY, [
        [-1,  1, -1, -1],     % [ 0,  1,  2,  0],
        [-1, -1, -1, -1],     % [ 0,  0,  2,  2],
        [-1, -1, -1, -1],     % [ 0,  0,  0,  2],
        [-1, -1,  1, -1]      % [ 0,  0,  1,  2],
        ], Output).

test_modules() :-
    test_snake_ends(),
    test_draw_path().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finding snake head & tail (a.k.a ends)

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
    draw_path_aux(Path, (NextX, Y), 
        [As|Bs], [[C|Cs]|Ds], Output).

test_draw_path() :-
    Path = [(0, 1)],
    Input = [[1, -1, 1], [-1, -1, -1], [-1, -1, -1]],
    Output = [[1, 2, 1], [0, 0, 0], [0, 0, 0]],
    draw_path(Path, Input, Output).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test that a path matches hints

test_path(Path, (HintsX, HintsY)) :-
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
    ( 
        arg(FixArg, Cell, FixVal) 
        -> NewCount is Count + 1
        , fix_count(Cells, FixArg, FixVal, NewCount, Output)
    ; 
        fix_count(Cells, FixArg, FixVal, Count, Output)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build a path from head to tail.

random_path([Start, Goal], Dim, Hints, Path)
    :- path(Start, Goal, Dim, Hints, [Start], Path).

path(Start, Goal, Dim, Hints, Path, Output) :- 
    (   
        % Goal is located linearly
        linear(Goal, Start, _)
        -> Output = [Goal|Path]
    ;
        % Goal is located diagonally
        diagonal(Goal, Start, (DX, DY))
        -> Start = (X, Y)
        , SX is X + DX, SY is Y + DY
        , member((GX, GY), [(SX, Y), (X, SY)])
        , Output = [Goal,(GX, GY)|Path]
    ;
        % Otherwise, ensure that the snake 
        % does not touch itself
        move2d(Start, Move, Dim),
        vicinity(Move, Path, 0, 1),
        path(Move, Goal, Dim, Hints, [Move|Path], Output)
    ).


vicinity(_, [], Num, Num).
vicinity(X, [Y|Ys], Num, Output) :-
    ( linear(X, Y, _) -> Next is Num + 1 
    ; Next = Num ),
    vicinity(X, Ys, Next, Output).

surrounds(Cell1, Cell2) :-
    linear(Cell1, Cell2, _), !;
    diagonal(Cell1, Cell2, _).

linear((X1, Y1), (X2, Y2), (DX, DY)) :-
    member((DX, DY), [(0, 1), (0, -1), (1, 0), (-1, 0)]),
    X1 is X2 + DX,
    Y1 is Y2 + DY, !.

diagonal((X1, Y1), (X2, Y2), (DX, DY)) :-
    member((DX, DY), [(1, 1), (1, -1), (-1, 1), (-1, -1)]),
    X1 is X2 + DX,
    Y1 is Y2 + DY, !.

% Move horizontally/vertically
move2d((X1, Y), (X2, Y), (MaxX, _)) :- move(X1, X2, MaxX).
move2d((X, Y1), (X, Y2), (_, MaxY)) :- move(Y1, Y2, MaxY).

% Move back/forward
move(X1, X2, Max) :- X2 is X1 + 1, X2 < Max.
move(X1, X2, _)   :- X2 is X1 - 1, X2 >= 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities

contains([], _) :- fail.
contains([X|Xs], Y) :- X == Y; contains(Xs, Y).

dimentions([], (0, 0)).
dimentions([X | Xs], (LenX, LenY)) :- 
    length([X|Xs], LenX),
    length(X, LenY), !.

unique(_, []).
unique(X, [Y|Ys]) :- X \= Y, unique(X, Ys).