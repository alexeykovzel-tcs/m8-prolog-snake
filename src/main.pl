:- [path].
:- [tests].
:- [utils].

snake(HintsX, HintsY, Grid, Output) :- 
    Hints = (HintsX, HintsY),
    find2d_all(Grid, 0, Empty),
    find2d_all(Grid, 1, [Head, Tail]),
    find2d_all(Grid, 2, Body),
    len2d(Grid, Dim), 
    !,
    build_path([Head, Tail], Hints, Dim, Path),
    check_path(Path, (Empty, Body), Hints),
    !,
    convert_path(Path, Grid, Output).