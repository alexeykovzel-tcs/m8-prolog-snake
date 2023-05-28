:- [utils].

% Order: 1-Right, 2-Up, 3-Left, 4-Down

move_order_grid(Grid, Hints, MoveOrders) :-
    map2d((0, 0), rate_cell, (Grid, Hints), Grid, Ratings),
    map2d((0, 0), move_order, Ratings, Ratings, MoveOrders).

move_order(Cell, Ratings, _, MoveOrder) :-
    move_rate( 1, 0, Cell, Ratings, 1, Right),
    move_rate( 0, 1, Cell, Ratings, 2, Up),
    move_rate(-1, 0, Cell, Ratings, 3, Left),
    move_rate( 0,-1, Cell, Ratings, 4, Down),
    sort([Right, Up, Left, Down], MoveRates),
    seconds(MoveRates, MoveOrder).

move_rate(DX, DY, (X1, Y1), Ratings, Direction, Rate) :-
    X2 is X1 + DX,
    Y2 is Y1 + DY,
    ( find2d(Ratings, R, (X2, Y2)) -> Rating = R
    ; Rating = -100 ),
    Rate = (Rating, Direction).

% Rate a cell from 0 to 100, where the higher the number, 
% the higher the probability the snake will go there
rate_cell((X, Y), (Grid, (HintsX, HintsY)), Tag, Rating) :-
    nth0(Y, HintsX, HintX),
    nth0(X, HintsY, HintY),
    row(X, Grid, Row),
    column(Y, Grid, Col),
    free_cells(Row, 0, FreeX),
    free_cells(Col, 0, FreeY),
    (   
        Tag == 2 -> Rating = 100;
        Tag == 1 -> Rating = 100;
        Tag == 0 -> Rating = 0;

        (HintX == -1 -> ValX = 3; ValX is (HintX / FreeX * 10)), 
        (HintY == -1 -> ValY = 3; ValY is (HintY / FreeY * 10)),
        Rating is ValX * ValY
    ).

% Find the number of cells that don't contain 0
free_cells([], Output, Output).
free_cells([X|Xs], Free, Output) :-
    (X \= 0 -> NextFree = Free + 1; NextFree = Free),
    free_cells(Xs, NextFree, Output).
