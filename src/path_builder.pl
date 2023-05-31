:- [utils].

% Building a path given hints and move orders
build_path([Start, Goal], Hints, Dim, MoveOrders, Path) :-
    Start = (X, Y),
    decr_hints(Start, Hints, StartHints),
    next_move(Start, Goal, StartHints, Dim, MoveOrders, 
        (X, X, Y, Y), [Start], Path).

next_move(Start, Goal, Hints, Dim, 
        MoveOrders, Bounds, Path, Output) :- 
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
        member(Dir, MoveOrder),
        move(Start, Pos, Dir),
        Path = [_|PathTail],
        not_collide(Pos, PathTail),
        update_bounds(Pos, Dir, Bounds, NewBounds),
        check_hints(Dir, Start, Goal, Hints, Bounds, Dim),
        decr_hints(Pos, Hints, LeftHints),
        next_move(Pos, Goal, LeftHints, Dim,
            MoveOrders, NewBounds, [Pos|Path], Output)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Order: 1-Right, 2-Up, 3-Left, 4-Down

update_bounds((X, Y), Dir, (MinX, MaxX, MinY, MaxY), NewBounds) :- 
    Dir == 1 -> NewMaxY is max(Y, MaxY), NewBounds = (MinX, MaxX, MinY, NewMaxY);
    Dir == 2 -> NewMinX is min(X, MinX), NewBounds = (NewMinX, MaxX, MinY, MaxY);
    Dir == 3 -> NewMinY is min(Y, MinY), NewBounds = (MinX, MaxX, NewMinY, MaxY);
    Dir == 4 -> NewMaxX is max(X, MaxX), NewBounds = (MinX, NewMaxX, MinY, MaxY).

test_free_space() :-
    Dir = 4,
    Pos = (2, 3),
    Goal = (2, 0), 
    Hints = (
        [-1, -1,  1, -1], 
        [-1, -1, -1,  1]
    ),
    Bounds = (0, 2, 1, 3),
    Dim = (4, 4),
    check_hints(Dir, Pos, Goal, Hints, Bounds, Dim).

check_hints(Dir, (X, Y), Goal, (HintsX, HintsY), Bounds, Dim) :-
    behind_hint_x(Dir, Goal, X, HintsX) -> has_space_x(Bounds, Dim);
    behind_hint_y(Dir, Goal, Y, HintsY) -> has_space_y(Bounds, Dim);
    true.

has_space_x((MinX, MaxX, MinY, MaxY), (_, DimY)) :-
    DimDiff is DimY - MaxY,
    (MinY >= 4; DimDiff >= 5
    % ; (MinY >= 2, DimDiff >= 3)
    ).

has_space_y((MinX, MaxX, MinY, MaxY), (DimX, _)) :- 
    DimDiff is DimX - MaxX,
    (MinX >= 4; DimDiff >= 5
    % ; (MinX >= 2, DimDiff >= 3)
    ).

behind_hint_x(Dir, (GX, _), X, HintsX) :-
    (Dir == 2, GX < X, left_after(X, HintsX), !); 
    (Dir == 4, GX > X, left_before(X, HintsX)).

behind_hint_y(Dir, (_, GY), Y, HintsY) :-
    (Dir == 3, GY < Y, left_after(Y, HintsY), !); 
    (Dir == 1, GY > Y, left_before(Y, HintsY)).

left_after(0, [A|As]) :- 
    !, A > 0 ; left_after(0, As).

left_after(X, [_|As]) :- 
    NextX is X - 1,
    left_after(NextX, As).

left_before(X, [A|As]) :- 
    A > 0, ! 
    ; X \= 0, 
      NextX is X - 1, 
      left_before(NextX, As).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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