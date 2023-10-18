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


%_______________________________________________________________________________________


% get all possible coords for a pawn, where not empty
% valid_pawn_coords(+Board, +X1, +Y1, ?X2, ?Y2)
valid_pawn_coords(Board, X1, Y1, X2, Y2) :-
    X2 = X1,
    Y2 is (Y1 + 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_pawn_coords(Board, X1, Y1, X2, Y2) :-    
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

%_______________________________________________________________________________________

% get all possible coords for a rook, where not empty
% moves any number of squares orthogonally on the first tower in its path.
% valid_rook_coords(+Board, +X1, +Y1, +Direction)
/* valid_rook_coords(Board, X1, Y1, X2, Y2, Dir):-
    % get Y size of board
    length(Board, YSize),
    % get X size of board
    nth0(0, Board, Row),
    length(Row, XSize),

    % check if coords are valid
    X2 >= 0,
    X2 < XSize,
    Y2 >= 0,
    Y2 < YSize.
    %TODO:

valid_rook_coords(Board, X1, Y1, X2, Y2, down) :-
    X2 = X1,
    Y2 is (Y1 + 1),
    valid_rook_coords(Board, X1, Y2, X2, Y2, down).
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.
    
valid_rook_coords(Board, X1, Y1, X2, Y2, up) :-
    X2 = X1,
    Y2 is (Y1 - 1),
    valid_rook_coords(Board, X1, Y2, X2, Y2, up).
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_rook_coords(Board, X1, Y1, X2, Y2, left) :-
    X2 is (X1 - 1),
    Y2 = Y1,
    valid_rook_coords(Board, X2, Y1, X2, Y2, left).
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_rook_coords(Board, X1, Y1, X2, Y2, right) :-
    X2 is (X1 + 1),
    Y2 = Y1,
    valid_rook_coords(Board, X2, Y1, X2, Y2, right).
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty. */

%_____________________________________________________________

% get all possible coords for a knight, where not empty
% valid_knight_coords(+Board, +X1, +Y1, ?X2, ?Y2)
valid_knight_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 + 1),
    Y2 is (Y1 + 2),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_knight_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 + 2),
    Y2 is (Y1 + 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_knight_coords(Board, X1, Y1, X2, Y2) :-    
    X2 is (X1 + 1),
    Y2 is (Y1 - 2),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.  

valid_knight_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 - 1),
    Y2 is (Y1 + 2),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_knight_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 + 2),
    Y2 is (Y1 - 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_knight_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 - 1),
    Y2 is (Y1 - 2),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_knight_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 - 2),
    Y2 is (Y1 + 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

valid_knight_coords(Board, X1, Y1, X2, Y2) :-
    X2 is (X1 - 2),
    Y2 is (Y1 - 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

%______________________________________________________________________________

% get all possible coords for a knight, where not empty
% knight_coords(+Board, +X1, +Y1, -Result)
knight_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_knight_coords(Board, X1, Y1, X2, Y2), Result).

% get all possible coords for a rook, where not empty
% rook_coords(+Board, +X1, +Y1, -Result)
rook_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_rook_coords(Board, X1, Y1, X2, Y2, _Dir), Result).

% get all possible coords for a pawn, where not empty
% pawn_coords(+Board, +X1, +Y1, -Result)
pawn_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_pawn_coords(Board, X1, Y1, X2, Y2), Result).

%______________________________________________________________________________

% move piece of size N
% move_piece(+Board, +X1, +Y1, +X2, +Y2, +N, -NewBoard)
move_piece(Board, X1, Y1, X2, Y2, 1, NewBoard):-
    move_pawn(Board, X1, Y1, X2, Y2, NewBoard).

move_piece(Board, X1, Y1, X2, Y2, 2, NewBoard):-
    move_rook(Board, X1, Y1, X2, Y2, NewBoard).

move_piece(Board, X1, Y1, X2, Y2, 3, NewBoard):-
    move_knight(Board, X1, Y1, X2, Y2, NewBoard).

move_piece(Board, X1, Y1, X2, Y2, 4, NewBoard):-
    move_bishop(Board, X1, Y1, X2, Y2, NewBoard).

move_piece(Board, X1, Y1, X2, Y2, 5, NewBoard):-
    move_queen(Board, X1, Y1, X2, Y2, NewBoard).


%______________________________________________________________________________


% move pawn to a given position
% move_pawn(+Board, +X1, +Y1, +X2, +Y2, -NewBoard)
% move_pawn(Board, X1, Y1, X2, Y2, NewBoard) :-
    % valid_pawn_coords(X1, Y1, X2, Y2),