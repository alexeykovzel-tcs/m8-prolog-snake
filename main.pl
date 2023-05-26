% :- [tests].

% snake(RowClues, ColClues, Grid, Solution) :- 
%     ones(Grid, [Head, Tail]),
%     len2d(Grid, Dim).

% complete(X, X, _, _, _) :- check_path(Path, Hints).
% complete(Head, Tail, Dim, Grid, Hints) :- 
%     path(Head, Tail, Dim, Path).

ones(Grid, Ones) :-
    Goal = (nth0(Y, Grid, Row), nth0(X, Row, 1)),
    findall((X, Y), Goal, Ones).

check_path(Path, HintsX, HintsY) :-
    check_hints(Path, HintsX, 0, first),
    check_hints(Path, HintsY, 0, second).

check_hints(_, [-1|_], _, _).
check_hints(Path, [Hint|Hints], Level, Extract) :-
    check_hint(Path, Extract, Level, Hint),
    NextLevel is Level + 1,
    check_hints(Path, Hints, NextLevel, Extract).

check_hint([], _, _, 0).
check_hint((Pos|Path), Extract, Level, Hint) :-
    call(Extract, Pos, Coord),
    Coord == Level,
    HintLeft is Hint - 1,
    check_hint(Path, Extract, Level, HintLeft), !.

%%%%%%%%%%%%%% PATH %%%%%%%%%%%%%%
% Completing path from head to tail.

path(Pos, Goal, Dim, Path) 
    :- path_aux(Pos, Goal, Dim, [], Path).

path_aux(Pos, Pos, _, Path, Path).
path_aux(Pos, Goal, Dim, Path, Result) :- 
    move2d(Pos, NextPos, Dim),
    unique(NextPos, Path),
    path_aux(NextPos, Goal, Dim, [NextPos|Path], Result).

% Move diagonally
move2d((X1, Y1), (X2, Y2), (MaxX, MaxY)) :-
    move(X1, X2, MaxX),
    move(Y1, Y2, MaxY).

% Move horizontally / vertically
move2d((X1, Y), (X2, Y), (MaxX, _)) :- move(X1, X2, MaxX).
move2d((X, Y1), (X, Y2), (_, MaxY)) :- move(Y1, Y2, MaxY).

% Move back / forward
move(X1, X2, Max) :- X2 is X1 + 1, X2 < Max.
move(X1, X2, _)   :- X2 is X1 - 1, X2 >= 0.

%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%

len(Xs, Len) :- len_aux(Xs, 0, Len).
len_aux([], Len, Len).
len_aux([_|Xs], Len, Result) :-
    NextLen is Len + 1,
    len_aux(Xs, NextLen, Result).

len2d([], (0, 0)).
len2d([X | Xs], (LenX, LenY)) :- 
    len([X|Xs], LenX),
    len(X, LenY).

unique(_, []).
unique(X, [Y|Ys]) :- X \= Y, unique(X, Ys).

first((X, _), X).
second((_, X), X).