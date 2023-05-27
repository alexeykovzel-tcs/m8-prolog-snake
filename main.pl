:- [tests].

snake(HintsX, HintsY, Input, Output) :- 
    Hints = (HintsX, HintsY),
    find_cells(Input, 0, Empty),
    find_cells(Input, 1, Ends),
    find_cells(Input, 2, Body),
    dimentions(Input, Dim),
    build_path(Ends, Dim, Hints, Path),
    test_path(Path, Input, (Empty, Body), Hints),
    draw_path(Path, Input, Output), !.

test_snake() :-
    HintsX = [-1, -1, -1, -1],
    HintsY = [-1,  2, -1,  3],
    snake(HintsX, HintsY, [
        [-1,  1,  0,  0],     % [ 0,  1,  0,  0],
        [-1, -1, -1,  2],     % [ 0,  2,  2,  2],
        [-1, -1, -1,  2],     % [ 0,  0,  0,  2],
        [-1, -1,  1, -1]      % [ 0,  0,  1,  2],
        ], Output),
    print_only_grid(Output).

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
    ; member((Y, X), Path) -> C = 2
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

test_path(Path, Grid, Prefilled, (HintsX, HintsY)) :-
    test_hints(Path, 1, HintsX, 0),
    test_hints(Path, 2, HintsY, 0),
    test_prefilled(Path, Prefilled).

% Test that a path respects prefilled cells
test_prefilled(Path, (Empty, Body)) :-
    maplist([BodyCell]>>(member(BodyCell, Path)), Body),
    maplist([EmptyCell]>>(\+ member(EmptyCell, Path)), Empty).

test_hints(_, _, [], _).
test_hints(Path, FixArg, [-1|Hints], Level) :-
    NextLevel is Level + 1,
    test_hints(Path, FixArg, Hints, NextLevel).

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

build_path([Start, Goal], Dim, Hints, Path) :-
    next_move(Start, Goal, Dim, Hints, [Start], Path).

next_move(Start, Goal, Dim, Hints, Path, Output) :- 
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
        next_move(Move, Goal, Dim, Hints, [Move|Path], Output)
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

traverse(_, [], _).
traverse((X, Y), [[]|Ys], Pred) :-
    NextY is Y + 1,
    traverse((X, NextY), Ys, Pred).

traverse((X, Y), [[A|As]|Bs], Pred) :-
    call(Pred, A, (X, Y)),
    NextX is X + 1,
    traverse((NextX, Y), [As|Bs], Pred).

find_cells(Grid, Tag, Moves) :-    
    Xs = nth0(X, Grid, Row),
    Ys = nth0(Y, Row, Tag),
    findall((X, Y), (Xs, Ys), Moves), !.

dimentions([], (0, 0)).
dimentions([X | Xs], (LenX, LenY)) :- 
    length([X|Xs], LenX),
    length(X, LenY), !.

unique(_, []).
unique(X, [Y|Ys]) :- X \= Y, unique(X, Ys).