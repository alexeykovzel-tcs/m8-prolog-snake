:- [utils].

% Building a path given hints and move orders
build_path([Start, Goal], Dim, Hints, MoveOrders, Path) :-
    next_move(Start, Goal, Dim, Hints, MoveOrders, [Start], Path).

next_move(Start, Goal, Dim, Hints, MoveOrders, Path, Output) :- 
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
        move(Start, Move, Dim, Direction),
        decr_hints(Move, Hints, LeftHints),
        vicinity(Move, Path, 0, 1),
        next_move(Move, Goal, Dim, LeftHints,
            MoveOrders, [Move|Path], Output)
    ).

decr_hints((X, Y), (HintsX, HintsY), (LeftHintsX, LeftHintsY)) :-
    decr_at(X, HintsX, LeftHintsX, X2), X2 \= -1, !,
    decr_at(Y, HintsY, LeftHintsY, Y2), Y2 \= -1, !.

vicinity(_, [], Num, Num).
vicinity(X, [Y|Ys], Num, Output) :-
    ( linear(X, Y, _) -> Next is Num + 1; Next = Num ),
    vicinity(X, Ys, Next, Output).

linear((X1, Y1), (X2, Y2), (DX, DY)) :-
    member((DX, DY), [(0, 1), (0, -1), (1, 0), (-1, 0)]),
    X1 is X2 + DX,
    Y1 is Y2 + DY, !.

diagonal((X1, Y1), (X2, Y2), (DX, DY)) :-
    member((DX, DY), [(1, 1), (1, -1), (-1, 1), (-1, -1)]),
    X1 is X2 + DX,
    Y1 is Y2 + DY, !.

% Order: 1-Right, 2-Up, 3-Left, 4-Down
move((X1, Y), (X2, Y), (MaxX, _), 1) :- movef(X1, X2, MaxX).
move((X, Y1), (X, Y2), (_, MaxY), 2) :- movef(Y1, Y2, MaxY).
move((X1, Y), (X2, Y), (_, _), 3) :- moveb(X1, X2).
move((X, Y1), (X, Y2), (_, _), 4) :- moveb(Y1, Y2).

movef(X1, X2, Max) :- X2 is X1 + 1, X2 < Max.
moveb(X1, X2)      :- X2 is X1 - 1, X2 >= 0.