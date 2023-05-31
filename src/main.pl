:- [move_order].
:- [path_builder].
:- [tests].
:- [utils].

snake(HintsX, HintsY, Grid, Output) :- 
    Hints = (HintsX, HintsY),
    find2d_all(Grid, 0, Empty),
    find2d_all(Grid, 1, [Head, Tail]),
    find2d_all(Grid, 2, Body),
    move_order_grid(Head, Grid, Hints, MoveOrder),
    len2d(Grid, Dim), 
    !,
    build_path([Head, Tail], Hints, Dim, MoveOrder, Path),
    check_path(Path, (Empty, Body), Hints),
    !,
    convert_path(Path, Grid, Output).

test_puzzle(Puzzle) :-
    puzzle(Puzzle, HintsX, HintsY, Grid),
    Hints = (HintsX, HintsY),
    nl, write('hintsX: '), write(HintsX),
    nl, write('hintsY: '), writeln(HintsY),
    nl, writeln('initial grid:'),
    print2d(Grid),
    find2d_all(Grid, 1, [Head, _]),
    map2d((0, 0), rate_cell, (Head, Grid, Hints), Grid, Ratings),
    nl, writeln('ratings:'),
    print2d(Ratings),
    map2d((0, 0), smart_order, Ratings, Ratings, MoveOrders),
    nl, writeln('move orders: (1-Right, 2-Up, 3-Left, 4-Down)'),
    print3d(MoveOrders),
    snake(HintsX, HintsY, Grid, Output),
    nl, writeln('output:'),
    print_only_grid(Output).