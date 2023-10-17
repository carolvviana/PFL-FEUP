:- consult('draw.pl').
:- use_module(library(lists)).

% get the piece at a given position
% get_piece(+Board, +X, +Y, ?Piece)
get_piece(Board, X, Y, Piece) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Piece).

% place a piece at a given position if empty
% place_piece(+Board, +X, +Y, +Piece, -NewBoard)
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

valid_pawn_coords(Board, X1, Y1, X2, Y2) :-
    X2 = X1,
    Y2 is (Y1 + 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_pawn_coords(Board,X1, Y1, X2, Y2) :-    
    X2 = X1,
    Y2 is (Y1 - 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.  

valid_pawn_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 + 1),
    Y2 = Y1,
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_pawn_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 - 1),
    Y2 = Y1,
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

% get all possible coords for a pawn, where not empty
% % pawn_coords(+Board, +X1, +Y1, -Result)
pawn_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_pawn_coords(Board, X1, Y1, X2, Y2), Result).

% valid_pawn_coords_rec(Board, X1, Y1, Result):-
%     valid_pawn_coords(X1, Y1, X2, Y2),
%     get_piece(Board, X2, Y2, empty),
%     valid_pawn_coords_rec(Board, X1, Y1, [X2-Y2 | Result]).

% move pawn to a given position
% move_pawn(+Board, +X1, +Y1, +X2, +Y2, -NewBoard)
% move_pawn(Board, X1, Y1, X2, Y2, NewBoard) :-
    % valid_pawn_coords(X1, Y1, X2, Y2),