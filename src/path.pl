:- [utils].

% Build a path given hints, starting and finishing cells,
% and dimentions of the grid
build_path([Start, Goal], Hints, Dim, Path) :-
    decr_hints(Start, Hints, StartHints),
    next_move(Start, Goal, StartHints, Dim, [Start], Path).

next_move(Start, Goal, Hints, Dim, Path, Output) :-
    (   
        % Goal is located linearly
        linear(Goal, Start)
        -> Output = [Goal|Path]
    ;
        % Goal is located diagonally
        diagonal(Goal, Start, (DX, DY))
        -> Start = (X, Y)
        , SX is X + DX, SY is Y + DY
        , member((GX, GY), [(SX, Y), (X, SY)])
        , Output = [Goal,(GX, GY)|Path]
    ;
        % Otherwise, a free move
        move_order(Path, Dim, Order),
        member(Dir, Order),
        move(Start, Pos, Dir),
        Path = [_|PathTail],
        not_collide(Pos, PathTail),
        decr_hints(Pos, Hints, LeftHints),
        next_move(Pos, Goal, LeftHints, 
            Dim, [Pos|Path], Output)
    ).

% Decrement hints corresponding to the move 
decr_hints((X, Y), (HintsX, HintsY), (LeftHintsX, LeftHintsY)) :-
    decr_at(X, HintsX, LeftHintsX, X2), X2 \= -1, !,
    decr_at(Y, HintsY, LeftHintsY, Y2), Y2 \= -1, !.

% Check that a cell doesn't collide with other cells
not_collide(_, []).
not_collide(Pos, [PrevPos|Path]) :-
    \+ linear(Pos, PrevPos),
    not_collide(Pos, Path).    

% Check if cells are located linearly
linear((X1, Y1), (X2, Y2)) :-
    member((DX, DY), [(0, 1), (0, -1), (1, 0), (-1, 0)]),
    X1 is X2 + DX,
    Y1 is Y2 + DY, !.

% Check if cells are located diagonally
diagonal((X1, Y1), (X2, Y2), (DX, DY)) :-
    member((DX, DY), [(1, 1), (1, -1), (-1, 1), (-1, -1)]),
    X1 is X2 + DX,
    Y1 is Y2 + DY, !.

% Order: 1-Right, 2-Up, 3-Left, 4-Down
move((X, Y1), (X, Y2), 1) :- Y2 is Y1 + 1.
move((X1, Y), (X2, Y), 2) :- X2 is X1 - 1.
move((X1, Y), (X2, Y), 4) :- X2 is X1 + 1.
move((X, Y1), (X, Y2), 3) :- Y2 is Y1 - 1.

% Determine the order of moves for a given path
% e.g. [1, 2] means go right, then go up 
move_order(Path, Dim, Order) :-
    Moves = [(1, 0, 1), (2, -1, 0), (3, 0, -1), (4, 1, 0)],
    findall((Rating, Dir),
        (member((Dir, DX, DY), Moves),
            Path = [(X,Y)|PathTail],
            X1 is X + DX,
            Y1 is Y + DY,
            Move = (X1, Y1),
            \+ touch_itself(Move, PathTail),
            \+ outside_grid(Move, Dim),
            rating(Move, Rating)
        ),
        Ratings
    ),
    sort(Ratings, Sorted),
    reverse(Sorted, SortedDesc),
    seconds(SortedDesc, Order).

% Check that a snake doesn't go outside the grid
outside_grid((X, Y), (DimX, DimY)) :- 
    X == -1; X == DimX; Y == -1; Y == DimY.

% Check that a snake does not touch itself
touch_itself((X1, Y1), [(X2, Y2)|_]) :-
    X1 == X2, Y1 == Y2.

rating(_, Rating) :-
    % We tried to assign ratings to moves to determine their order.
    % However, we could not come up with an efficient solution.
    Rating = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check if a path respects hints and prefilled values

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
% Convert a path to the final grid

convert_path(Path, Input, Output) :-
    map2d((0, 0), convert_cell, Path, Input, Output).

convert_cell((X, Y), Path, A, B) :-
    A == 1 -> B = 1; 
    member((Y, X), Path) -> B = 2; 
    B = 0.