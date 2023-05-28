:- [utils].

% Convert a path to a grid
convert_path(Path, Input, Output) :-
    convert_path_aux(Path, (0, 0), Input, [[]], Output), !.    

convert_path_aux(_, _, [], IR1, Output) :-
    maplist(reverse, IR1, IR2),
    reverse(IR2, IR3),
    exclude(=([]), IR3, Output).

convert_path_aux(Path, (_, Y), [[]|Bs], Ds, Output) :-
    NextY is Y + 1,
    convert_path_aux(Path, (0, NextY), Bs, [[]|Ds], Output).

convert_path_aux(Path, (X, Y), [[A|As]|Bs], [Cs|Ds], Output) :-
    ( A == 1 -> C = 1
    ; member((Y, X), Path) -> C = 2
    ; C = 0 ),
    NextX is X + 1,
    convert_path_aux(Path, (NextX, Y), 
        [As|Bs], [[C|Cs]|Ds], Output).

test_convert_path() :-
    Path = [(0, 1)],
    Input = [[1, -1, 1], [-1, -1, -1], [-1, -1, -1]],
    Output = [[1, 2, 1], [0, 0, 0], [0, 0, 0]],
    convert_path(Path, Input, Output).