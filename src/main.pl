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