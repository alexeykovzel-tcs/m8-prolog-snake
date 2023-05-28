:- [utils].

% Building a path given hints and move orders
build_path([Start, Goal], Hints, MoveOrders, Path) :-
    next_move(Start, Goal, Hints, MoveOrders, [Start], Path).

next_move(Start, Goal, Hints, MoveOrders, Path, Output) :- 
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
        % Otherwise, free move
        find2d(MoveOrders, MoveOrder, Start),
        member(Direction, MoveOrder),
        move(Start, Pos, Direction),
        Path = [_|PathTail],
        not_collide(Pos, PathTail),
        decr_hints(Pos, Hints, LeftHints),
        next_move(Pos, Goal, LeftHints,
            MoveOrders, [Pos|Path], Output)
    ).

decr_hints((X, Y), (HintsX, HintsY), (LeftHintsX, LeftHintsY)) :-
    decr_at(X, HintsX, LeftHintsX, X2), X2 \= -1, !,
    decr_at(Y, HintsY, LeftHintsY, Y2), Y2 \= -1, !.

not_collide(_, []).
not_collide(Pos, [PrevPos|Path]) :-
    \+ linear(Pos, PrevPos, _),
    not_collide(Pos, Path).    

linear((X1, Y1), (X2, Y2), (DX, DY)) :-
    member((DX, DY), [(0, 1), (0, -1), (1, 0), (-1, 0)]),
    X1 is X2 + DX,
    Y1 is Y2 + DY, !.

diagonal((X1, Y1), (X2, Y2), (DX, DY)) :-
    member((DX, DY), [(1, 1), (1, -1), (-1, 1), (-1, -1)]),
    X1 is X2 + DX,
    Y1 is Y2 + DY, !.

% Order: 1-Right, 2-Up, 3-Left, 4-Down
move((X, Y1), (X, Y2), 1) :- Y2 is Y1 + 1.
move((X1, Y), (X2, Y), 2) :- X2 is X1 - 1.
move((X1, Y), (X2, Y), 4) :- X2 is X1 + 1.
move((X, Y1), (X, Y2), 3) :- Y2 is Y1 - 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check if path respects hints and prefilled values
check_path(Path, Prefilled, (HintsX, HintsY)) :-
    check_hints(Path, 1, HintsX, 0),
    check_hints(Path, 2, HintsY, 0),
    check_prefilled(Path, Prefilled).

check_prefilled(Path, (Empty, Body)) :-
    maplist([BodyCell]>>(member(BodyCell, Path)), Body),
    maplist([EmptyCell]>>(\+ member(EmptyCell, Path)), Empty).

check_hints(_, _, [], _).
check_hints(Path, Axis, [Hint|Hints], Level) :-
    ( Hint \= -1 -> count_tuples(Path, Axis, Level, Hint); true),
    NextLevel is Level + 1,
    check_hints(Path, Axis, Hints, NextLevel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert path to a grid
convert_path(Path, Input, Output) :-
    map2d((0, 0), convert_cell, Path, Input, Output).

convert_cell((X, Y), Path, A, B) :-
    A == 1 -> B = 1; 
    member((Y, X), Path) -> B = 2; 
    B = 0.