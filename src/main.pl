:- [move_order].
:- [path_builder].
:- [path_checker].
:- [path_converter].
:- [tests].
:- [utils].

snake(HintsX, HintsY, Grid, Output) :- 
    Hints = (HintsX, HintsY),
    find_cells(Grid, 0, Empty),
    find_cells(Grid, 1, Ends),
    find_cells(Grid, 2, Body),
    move_order_grid(Grid, Hints, MoveOrder), 
    dimentions(Grid, Dim), !,
    build_path(Ends, Dim, Hints, MoveOrder, Path),
    check_path(Path, (Empty, Body), Hints),
    convert_path(Path, Grid, Output), !.

test_puzzle(Hints, Grid) :-
    Hints = (
    [-1, -1,  2, -1],
    [-1, -1, -1,  3]), 
    Grid = [    
    [-1,  1, -1, -1],     % [ 0,  1,  2,  0],
    [-1, -1, -1, -1],     % [ 0,  0,  2,  2],
    [ 1, -1, -1, -1],     % [ 1,  0,  0,  2],
    [-1, -1,  2, -1]].    % [ 2,  2,  2,  2],

test_snake() :-
    test_puzzle((HintsX, HintsY), Input),
    snake(HintsX, HintsY, Input, Output),
    print_only_grid(Output).