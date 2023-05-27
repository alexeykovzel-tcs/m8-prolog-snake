:- [tests].

snake(HintsX, HintsY, Grid, Output) :- 
    Hints = (HintsX, HintsY),
    find_cells(Grid, 0, Empty),
    find_cells(Grid, 1, Ends),
    find_cells(Grid, 2, Body),
    dimentions(Grid, Dim), !,
    build_path(Ends, Dim, Hints, Path),
    test_path(Path, Input, (Empty, Body), Hints),
    draw_path(Path, Grid, Output), !.

test_snake() :-
    HintsX = [-1, -1, -1, -1],
    HintsY = [-1,  2, -1,  3],
    snake(HintsX, HintsY, [
        [-1,  1,  0, -1],     % [ 0,  1,  0,  0],
        [-1, -1, -1, -1],     % [ 0,  2,  2,  2],
        [-1, -1, -1,  2],     % [ 0,  0,  0,  2],
        [-1, -1,  1, -1]      % [ 0,  0,  1,  2],
        ], Output),
    print_only_grid(Output).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate grid for deciding next moves

rate_grid(Grid, Dim, Hints, Ratings) :-
    grid_map((0, 0), rate_cell, (Hints, Dim), Grid, Ratings).

rate_cell(Grid, (X, Y), (Hints, (DimX, DimY)), Tag, Rating) :-
    nth0(X, HintsX, HintX),
    nth0(Y, HintsY, HintY),
    (   
        Tag == 2 -> Rating = 100;   
        Tag == 0 -> Rating = 0;

        (HintX == -1 -> ValX = 3; ValX is HintX / DimX * 10), 
        (HintY == -1 -> ValY = 3; ValY is HintY / DimY * 10),  
        Rating is ValX * ValY
    ).

grid_map(_, _, _, [], []).
grid_map((X, Y), Pred, Input, [[]|Bs], [[]|Ds]) :-
    NextY is Y + 1,
    grid_map((X, NextY), Pred, Input, Bs, Ds), !.

grid_map((X, Y), Pred, Input, [[A|As]|Bs], [[C|Cs]|Ds]) :-
    call(Pred, (X, Y), Input, A, C),
    NextX is X + 1,
    grid_map((NextX, Y), 
        Pred, Input, [As|Bs], [Cs|Ds]).

free_cells([], 0).
free_cells([X|Xs], Free) :-
    X == -1, !,
    free_cells(Xs, FreeAux),
    Free is FreeAux + 1;
    free_cells(Xs, Free).

get_row(N, Xs, Row) :-
    nth0(N, Xs, Row).

get_column([], _, []).
get_column([X|Xs], I, [Y|Ys]) :-
    nth0(I, X, Y),
    get_column(Xs, I, Ys).

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
        % Otherwise, ensure that the 
        % snake does not eat itself
        move2d(Start, Move, Dim),
        adjust_hints(Move, Hints, LeftHints),
        vicinity(Move, Path, 0, 1),
        next_move(Move, Goal, Dim, LeftHints, [Move|Path], Output)
    ).

adjust_hints((X, Y), (HintsX, HintsY), (LeftHintsX, LeftHintsY)) :-
    decrement_at(X, HintsX, LeftHintsX, X2),
    X2 \= -1, !,
    decrement_at(X, HintsX, LeftHintsY, Y2),
    Y2 \= -1, !.

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

decrement_at(Idx, List, NewList, Val) :-
    length(Before, Idx),
    append(Before, [X|After], List),
    Val is X - 1,
    append(Before, [Val|After], NewList).

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