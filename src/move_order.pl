:- [utils].

% Order: 1-Right, 2-Up, 3-Left, 4-Down

move_order_grid(Head, Grid, Hints, Orders) :-
    map2d((0, 0), rate_cell, (Head, Grid, Hints), Grid, Ratings),
    map2d((0, 0), smart_order, Ratings, Ratings, Orders).

% Rate a cell from 0 to 100, where the higher the number, rate_move
% the higher the probability the snake will go there
rate_cell(Cell, (Head, Grid, (HintsX, HintsY)), Tag, Rating) :-
    Rating = 0.
    % Cell = (X, Y),
    % nth0(Y, HintsX, HintX),
    % nth0(X, HintsY, HintY),
    % row(X, Grid, Row),
    % column(Y, Grid, Col),
    % free_cells(Row, 0, FreeX),
    % free_cells(Col, 0, FreeY),
    % (   
    %     Head == Cell -> Rating = 0;
    %     Tag == 2 -> Rating = 100;
    %     Tag == 1 -> Rating = 200;
    %     Tag == 0 -> Rating = 0;

    %     (HintX == -1 -> ValX = 2; ValX is (HintX / FreeX * 10)), 
    %     (HintY == -1 -> ValY = 2; ValY is (HintY / FreeY * 10)),
    %     Rating is round(ValX * ValY)
    % ).

free_cells([], Output, Output).
free_cells([X|Xs], Free, Output) :-
    (X \= 0 -> NextFree = Free + 1; NextFree = Free),
    free_cells(Xs, NextFree, Output).

% Define the order of moves based on cell ratings
smart_order(Cell, Ratings, _, Order) :-
    Moves = [(1, 1, 0), (2, 0, -1), (3, -1, 0), (4, 0, 1)],
    ratings_around(Moves, Cell, Ratings, [], A),
    sort(A, B), reverse(B, C), seconds(C, Order).

ratings_around([], _, _, Output, Output).
ratings_around([(Dir, DX, DY)|Ds], (X, Y), Ratings, Now, Output) :-
    RX is X + DX, 
    RY is Y + DY,
    ( find2d(Ratings, Rating, (RY, RX))
        -> Next = [(Rating, Dir)|Now]; 
        Next = Now ),
    ratings_around(Ds, (X, Y), 
        Ratings, Next, Output).