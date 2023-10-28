removehead([_|Tail], Tail).

reverse_between(High, Low, High) :- 
    High >= Low.

reverse_between(High, Low, X) :-
    High > Low,
    NewHigh is High - 1,
    reverse_between(NewHigh, Low, X).

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