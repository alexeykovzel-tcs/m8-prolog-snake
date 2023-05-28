:- [move_order].
:- [path_builder].
:- [tests].
:- [utils].

snake(HintsX, HintsY, Grid, Output) :- 
    Hints = (HintsX, HintsY),
    find2d_all(Grid, 0, Empty),
    find2d_all(Grid, 1, Ends),
    find2d_all(Grid, 2, Body),
    move_order_grid(Grid, Hints, MoveOrder), 
    len2d(Grid, Dim), !,
    build_path(Ends, Dim, Hints, MoveOrder, Path),
    check_path(Path, (Empty, Body), Hints),
    convert_path(Path, Grid, Output), !.

test_puzzle(Grid, Hints) :-
    Hints = (
        [-1, -1, -1, -1], 
        [-1,  2, -1,  3]
    ), Grid = [
        [-1,  1,  0, -1],     % [ 0,  1,  0,  0],
        [-1, -1, -1, -1],     % [ 0,  2,  2,  2],
        [-1, -1, -1,  2],     % [ 0,  0,  0,  2],
        [-1, -1,  1, -1]      % [ 0,  0,  1,  2],
    ].

test_move_order() :-
    test_puzzle(Grid, Hints),
    Hints = (HintsX, HintsY),
    map2d((0, 0), rate_cell, (Grid, Hints), Grid, Ratings),
    map2d((0, 0), move_order, Ratings, Ratings, MoveOrders),
    snake(HintsX, HintsY, Grid, Output),
    nl, writeln('initial grid:'),
    print2d(Grid),
    nl, writeln('ratings:'),
    print2d(Ratings),
    nl, writeln('move orders: (1-Right, 2-Up, 3-Left, 4-Down)'),
    print3d(MoveOrders),
    nl, writeln('output:'),
    print_only_grid(Output).