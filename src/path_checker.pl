:- [utils].

% Check that a path respects hints and prefilled values.
check_path(Path, Prefilled, (HintsX, HintsY)) :-
    check_hints(Path, 1, HintsX, 0),
    check_hints(Path, 2, HintsY, 0),
    check_prefilled(Path, Prefilled).

check_prefilled(Path, (Empty, Body)) :-
    maplist([BodyCell]>>(member(BodyCell, Path)), Body),
    maplist([EmptyCell]>>(\+ member(EmptyCell, Path)), Empty).

check_hints(_, _, [], _).
check_hints(Path, FixArg, [-1|Hints], Level) :-
    NextLevel is Level + 1,
    check_hints(Path, FixArg, Hints, NextLevel).

check_hints(Path, FixArg, [Expected|Hints], Level) :-
    fix_count(Path, FixArg, Level, 0, Actual),
    Expected = Actual,
    NextLevel is Level + 1,
    check_hints(Path, FixArg, Hints, NextLevel).

fix_count([], _, _, Count, Count).
fix_count([Cell|Cells], FixArg, FixVal, Count, Output) :-
    ( 
        arg(FixArg, Cell, FixVal) 
        -> NewCount is Count + 1
        , fix_count(Cells, FixArg, FixVal, NewCount, Output)
    ; 
        fix_count(Cells, FixArg, FixVal, Count, Output)
    ).