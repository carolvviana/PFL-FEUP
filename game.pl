:- consult('draw.pl').
:- consult('utils.pl').
:- use_module(library(lists)).

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
% moves any number of squares orthogonally on the first tower in its path (not empty).
% valid_rook_coords(+Board, +X1, +Y1, -X2, -Y2, +Direction)
% valid_rook_coords(Board, X1, Y1, X2, Y2, _) :-
%     get_piece(Board, X2, Y2, Piece),
%     Piece \= empty.
/*
valid_rook_coords(Board, X1, Y1, X2, Y2, up) :-
    NewY is (Y1 - 1),
    get_piece(Board, X1, NewY, Piece),
    Piece = empty,
    \+valid_rook_coords(Board, X1, NewY, X2, Y2, up), !,
    X2 is X1,
    Y2 is Y1 - 1.
    
valid_rook_coords(Board, X1, Y1, X2, Y2, down) :-
    NewY is (Y1 + 1),
    get_piece(Board, X1, NewY, Piece),
    Piece = empty,
    \+valid_rook_coords(Board, X1, NewY, X2, Y2, down), !,
    X2 is X1,
    Y2 is Y1 + 1.
    
valid_rook_coords(Board, X1, Y1, X2, Y2, left) :-
    NewX is (X1 - 1),
    get_piece(Board, NewX, Y1, Piece),
    Piece = empty,
    \+valid_rook_coords(Board, NewX, Y1, X2, Y2, left), !,
    Y2 is Y1,
    X2 is X1 - 1.
    
valid_rook_coords(Board, X1, Y1, X2, Y2, right) :-
    NewX is (X1 + 1),
    get_piece(Board, NewX, Y1, Piece),
    Piece = empty,
    \+valid_rook_coords(Board, NewX, Y1, X2, Y2, right), !,
    Y2 is Y1,
    X2 is X1 + 1.
*/
%_____________________________________________________________
    % get_piece(Board, NewX, NewY, Piece), !.
    % Piece = empty ->
    % (valid_rook_coords(Board, NewX, NewY, X2, Y2, Direction));
    % (X2 = NewX,
    % Y2 = NewY).

    
% valid_rook_coords(Board, X1, Y1, X2, Y2, Direction) :-
%     direction_offset(Direction, DX, DY),
%     NewX is X1 + DX,
%     NewY is Y1 + DY,
    
%     get_piece(Board, NewX, NewY, Piece),
%     Piece = empty,
%     (valid_rook_coords(Board, NewX, NewY, X2, Y2, Direction)
%     -> (X2 is NewX, Y2 is NewY)
%     ; false).

% valid_rook_coords(Board, X1, Y1, X2, Y2, Direction) :-
%     X2 is X1,
%     Y2 is Y1.

% valid_rook_coords(Board, X1, Y1, X1, Y1, Direction).
% valid_rook_coords(Board, X1, Y1, X2, Y2, Direction) :-
%     direction_offset(Direction, DX, DY),
%     X2 is X1 + DX,
%     Y2 is Y1 + DY,
    
%     get_piece(Board, NewX, NewY, Piece), 
%     (
%         Piece = empty -> valid_rook_coords(Board, X2, Y2, X3, Y3, Direction)
%         ; X2 is X3, Y2 is Y3
%     ).



% get all possible coords for a rook, where not empty
% moves any number of squares orthogonally on the first tower in its path (not empty).
% valid_rook_coords(+Board, +X1, +Y1, -X2, -Y2, +Direction)
% valid_rook_coords(Board, X1, Y1, X2, Y2, Direction) :-
%     direction_offset(Direction, DX, DY),
%     X2 is X1 + DX,
%     Y2 is Y1 + DY,

%     (   (get_piece(Board, X2, Y2, Piece); fail),
%         Piece \= empty
%     ->  true, !
%     ;   valid_rook_coords(Board, X2, Y2, X3, Y3, Direction),
%         X2 is X3,
%         Y2 is Y3
%     ).
direction_offset(up, 0, -1).
direction_offset(down, 0, 1).
direction_offset(left, -1, 0).
direction_offset(right, 1, 0).

% valid_rook_coords(Board, X1, Y1, X2, Y2, Direction) :-
%     direction_offset(Direction, DX, DY),
%     X2 is X1 + DX,
%     Y2 is Y1 + DY,
%     get_piece(Board, X2, Y2, empty),
%     valid_rook_coords(Board, X2, Y2, X3, Y3, Direction).
% valid_rook_coords(Board, X1, Y1, X2, Y2, Direction) :-
%     direction_offset(Direction, DX, DY),
%     X2 is X1 + DX,
%     Y2 is Y1 + DY,
%     get_piece(Board, X2, Y2, empty),
%     valid_rook_coords(Board, X2, Y2, X3, Y3, Direction),
%     X2 = X3,
%     Y2 = Y3.

valid_rook_coords(_, X, Y, X, Y, _).
valid_rook_coords(Board, X1, Y1, X2, Y2, Direction) :-
    direction_offset(Direction, DX, DY),
    X3 is X1 + DX,
    Y3 is Y1 + DY,
    get_piece(Board, X3, Y3, empty),
    valid_rook_coords(Board, X3, Y3, X2, Y2, Direction).

% get all possible coords for a rook, where not empty
% rook_coords(+Board, +X1, +Y1, -Result)
rook_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_rook_coords(Board, X1, Y1, X2, Y2, _Dir), Result).

/*
rook_coords([
    [empty, a, empty, empty],
    [empty, a, empty, empty],
    [empty, r, a, a],
    [empty, a, empty, empty]
], 1, 2, Result). Gives Result = [0-2] but should be [1-1, 1-3, 2-2]

rook_coords([
    [empty, a, empty, empty],
    [empty, empty, empty, empty],
    [empty, r, empty, a],
    [empty, a, empty, empty]
], 1, 2, Result). Gives Result = [1-1, 0-2, 2-2] but should be [0-1, 1-3, 3-2]

*/



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


% get all possible coords for a pawn, where not empty
% pawn_coords(+Board, +X1, +Y1, -Result)
pawn_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, (valid_pawn_coords(Board, X1, Y1, X2, Y2), X1-Y1 \= X2-Y2), Result).

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
