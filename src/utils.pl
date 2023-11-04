removehead([_|Tail], Tail).

% get the piece at a given position
% get_piece(+Board, +X, +Y, ?Piece)
get_piece(Board, X, Y, Piece) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Piece).

% place a piece at a given position if empty
% add_new_piece(+Board, +X, +Y, +Piece, -NewBoard)
add_new_piece(Board, X, Y, Piece, NewBoard) :-
    get_piece(Board, X, Y, empty),
    replace(Board, X, Y, Piece, NewBoard).

% replace a piece at a given position
% replace(+Board, +X, +Y, +Piece, -NewBoard)
replace(Board, X, Y, Piece, NewBoard) :-
    nth0(Y, Board, Row),
    replace_row(Row, X, Piece, NewRow),
    replace_row(Board, Y, NewRow, NewBoard).

% replace a piece in a row
% replace_row(+Row, +X, +Piece, -NewRow)
replace_row(Row, X, Piece, NewRow) :-
    nth0(X, Row, _, TempRow),
    nth0(X, NewRow, Piece, TempRow).

%_______________________________________________________________________________________

% clear the terminal
cls :- write('\33\[2J').

has_six_head([]) :- fail. % If the board is empty, fail
has_six_head([Row|Rest]) :- check_row(Row); has_six_head(Rest). % Check each row

check_row([]) :- fail. % If the row is empty, fail
check_row([Piece|Rest]) :- check_piece(Piece); check_row(Rest). % Check each piece

check_piece([Head|Piece]) :- is_list([Head|Piece]), Head >= 6. % If the piece is a list and its head is 6, succeed

removehead([_|Tail], Tail).

piece_size(Piece, Size) :- Piece = [Size|_].

largest_piece(Pieces, Largest) :-
    maplist(piece_size, Pieces, Sizes),
    list_max(Sizes, Largest).

list_max([P|T], Max) :- list_max(T, P, Max).

list_max([], P, P).
list_max([H|T], P, O) :-
    (    H > P
    ->   list_max(T, H, O)
    ;    list_max(T, P, O)
    ).

repeat_string(_, 0, '').
repeat_string(Str, N, Result):-
    N > 0,
    NewN is N - 1,
    repeat_string(Str, NewN, NewResult),
    atom_concat(Str, NewResult, Result).

print_coordinates([],_).
print_coordinates([X-Y|T], N) :-
    N1 is N+1,
    write(N), write('). '), write(X), write(','), write(Y), write('\n'),
    print_coordinates(T, N1).