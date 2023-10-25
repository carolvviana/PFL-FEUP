:- consult('draw.pl').
:- consult('utils.pl').
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
/*
valid_rook_coords(Board, X1, Y1, X2, Y2, down) :-
trace,
    length(Board, Length),
    X2 = X1,
    Y2 is (Y1 + 1),
    Y2 < Length,
    X2 < Length,
    valid_rook_coords(Board, X1, Y2, X2, Y2, down).
    %get_piece(Board, X2, Y2, Piece),
    %Piece \= empty.
    
% valid_rook_coords(Board, X1, Y1, X2, Y2, Dir):-
%     get_piece(Board, X2, Y2, Piece),
%     Piece \= empty.

% valid_rook_coords(Board, X1, Y1, X2, Y2, _):-
%     % get size of board
%     length(Board, Size),

%     % check if coords are valid
%     X2 >= 0,
%     X2 < Size,
%     Y2 >= 0,
%     Y2 < Size.

% valid_rook_coords(Board, X1, Y1, X2, Y2, down) :-
%     length(Board, Size),
%     X1 <= Size,
%     X1 >= 0,
%     Y1 <= Size,
%     Y1 >= 0,
%     X2 = X1,
%     Y2 is (Y1 + 1),
%     valid_rook_coords(Board, X1, Y2, X2, Y2, down).
%     %get_piece(Board, X2, Y2, Piece),
%     %Piece \= empty.
    
% valid_rook_coords(Board, X1, Y1, X2, Y2, up) :-
%     length(Board, Size),
%     X1 <= Size,
%     X1 >= 0,
%     Y1 <= Size,
%     Y1 >= 0,
%     X2 = X1,
%     Y2 is (Y1 - 1),
%     valid_rook_coords(Board, X1, Y2, X2, Y2, up).
%     %get_piece(Board, X2, Y2, Piece),
%     %Piece \= empty.

% valid_rook_coords(Board, X1, Y1, X2, Y2, left) :-
%     length(Board, Size),
%     X1 <= Size,
%     X1 >= 0,
%     Y1 <= Size,
%     Y1 >= 0,
%     X2 is (X1 - 1),
%     Y2 = Y1,
%     valid_rook_coords(Board, X2, Y1, X2, Y2, left).
%     % get_piece(Board, X2, Y2, Piece),
%     % Piece \= empty.

% valid_rook_coords(Board, X1, Y1, X2, Y2, right) :-
%     length(Board, Size),
%     X1 <= Size,
%     X1 >= 0,
%     Y1 <= Size,
%     Y1 >= 0,
%     X2 is (X1 + 1),
%     Y2 = Y1,
%     valid_rook_coords(Board, X2, Y1, X2, Y2, right).
%     % get_piece(Board, X2, Y2, Piece),
%     % Piece \= empty.

% valid_rook_coords(Board, X1, Y1, X2, Y2, _):-
%     get_piece(Board, X2, Y2 Piece),
%     Piece \= empty.

valid_rook_coords(Board, X1, Y1, X2, Y2, _) :-
    length(Board, Size),
    X1 >= 0, X1 < Size,
    Y1 >= 0, Y1 < Size,
    X2 >= 0, X2 < Size,
    Y2 >= 0, Y2 < Size,
    X1 \= X2,
    Y1 \= Y2,
    get_piece(Board, X2, Y2, Piece),
    Piece = empty.

valid_rook_coords(Board, X1, Y1, X2, Y2, up) :-
    length(Board, Size),
    X1 =< Size,
    X1 >= 0,
    Y1 =< Size,
    Y1 >= 0,
    Y2 is (Y1 - 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty,
    valid_rook_coords(Board, X1, Y2, X2, Y2, up).
    
valid_rook_coords(Board, X1, Y1, X2, Y2, down) :-
    length(Board, Size),

    X1 =< Size,
    X1 >= 0,
    Y1 =< Size,
    Y1 >= 0,
    Y2 is (Y1 + 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty,
    valid_rook_coords(Board, X1, Y2, X2, Y2, down).
    
valid_rook_coords(Board, X1, Y1, X2, Y2, left) :-
    length(Board, Size),
    X1 =< Size,
    X1 >= 0,
    Y1 =< Size,
    Y1 >= 0,
    X2 is (X1 - 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty,
    valid_rook_coords(Board, X2, Y1, X2, Y2, left).
    
valid_rook_coords(Board, X1, Y1, X2, Y2, right) :-
    length(Board, Size),
    X1 =< Size,
    X1 >= 0,
    Y1 =< Size,
    Y1 >= 0,
    X2 is (X1 + 1),
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty,
    valid_rook_coords(Board, X2, Y1, X2, Y2, right).


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

% get all possible coords for a queen, where not empty
% valid_queen_coords(+Board, +X1, +Y1, ?X2, ?Y2)
valid_queen_coords(Board, X1, Y1, X2, Y2) :-
    valid_rook_coords(Board, X1, Y1, X2, Y2, _Dir).

valid_queen_coords(Board, X1, Y1, X2, Y2) :-
    valid_bishop_coords(Board, X1, Y1, X2, Y2, _Dir).

%_____________________________________________________

valid_coords(Board, X, Y, 1, Result):-
    pawn_coords(Board, X, Y, Result).

valid_coords(Board, X, Y, 2, Result):-
    rook_coords(Board, X, Y, Result).

valid_coords(Board, X, Y, 3, Result):-
    knight_coords(Board, X, Y, Result).


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

split(Piece, N, NewHead, NewTail) :-
    length(NewTail, N),
    append(NewHead, NewTail, Piece).

normalise([0], empty).
normalise(Old,Old):- dif(Old, [0]).

% move N disks from piece at (X1,Y1) to piece at (X2,Y2)
% move_piece(+Board, +X1, +Y1, +X2, +Y2, +N, -NewBoard)
move_piece(Board, X1, Y1, X2, Y2, N, NewBoard) :-

    get_piece(Board, X1, Y1, Piece), %obter a peça na posição inicial
    nth0(0, Piece, T), %obter o número de discos da peça 

    Piece = [_|NPiece], %remover primeiro elemento
   
    split(NPiece, N, NOld, NNew), %dividir a peça em duas

    T1 is T-N, %obter o tamanho da peça depois de mover
    Old = [T1|NOld], %append ao novo tamanho

    get_piece(Board, X2, Y2, NewPiece), %obter a peça na posição inicial
    nth0(0, NewPiece, X), %obter o número de discos da peça 
    delete(NewPiece, X, NewPiece1), %remover primeiro elemento
    T2 is X+N, %obter o tamanho da peça depois de mover

    NewPiece2 = [T2|NewPiece1], %colocar o tamanho da peça depois de mover
    
    append([NewPiece2, NNew], New), %colocar o tamanho da peça depois de mover

    normalise(Old, Old1), %normalizar a peça (passar par empty se for [0])

    replace(Board, X1, Y1, Old1, NBoard), %colocar a peça na posição inicial
    replace(NBoard, X2, Y2, New, NewBoard). %colocar a peça na posição final


%______________________________________________________________________________
