:- consult('board.pl').
:- consult('utils.pl').
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

%______________________________________________________________________________

% Predicate that gives dx and dy for each direction on the board
% direction(+Direction, ?DX, ?DY)
direction(up, 0, -1).
direction(down, 0, 1).
direction(left, -1, 0).
direction(right, 1, 0).
direction(up_right, 1, -1).
direction(down_right, 1, 1).
direction(down_left, -1, 1).
direction(up_left, -1, -1).

%______________________________________________________________________________

% get all possible coords for a pawn, where not empty
% valid_pawn_coords(+Board, +X1, +Y1, ?X2, ?Y2, +Direction)
valid_pawn_coords(Board, X1, Y1, X2, Y2, Direction) :-
    direction(Direction, DX, DY),
    X2 is X1 + DX,
    Y2 is Y1 + DY,
    get_piece(Board, X2, Y2, Piece),
    Piece \= empty.

%_______________________________________________________________________________________

% get all possible coords for a rook or a bishop, where not empty
% moves any number of squares orthogonally on the first tower in its path (not empty).
% valid_recursive_coords(+Board, +X1, +Y1, -X2, -Y2, +Direction).
valid_recursive_coords(Board, X1, Y1, X2, Y2, Direction) :-
    length(Board, N),
    Size is N - 1,

    direction(Direction, DX, DY),

    between(1, Size, N1),
    X2 is X1 + DX*N1,
    Y2 is Y1 + DY*N1,

    get_piece(Board, X2, Y2, Piece),
    dif(Piece, empty), !.

%_______________________________________________________________________________________

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

% get all possible coords for a pawn
%pawn_coords(+Board, +X1, +Y1, -Result)
pawn_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, 
        (
            valid_pawn_coords(Board, X1, Y1, X2, Y2, up);
            valid_pawn_coords(Board, X1, Y1, X2, Y2, down);
            valid_pawn_coords(Board, X1, Y1, X2, Y2, left);
            valid_pawn_coords(Board, X1, Y1, X2, Y2, right)
        ), Result).

%______________________________________________________________________________

% get all possible coords for a rook, where not empty
% rook_coords(+Board, +X1, +Y1, -Result)
rook_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, 
        (
            valid_recursive_coords(Board, X1, Y1, X2, Y2, right);
            valid_recursive_coords(Board, X1, Y1, X2, Y2, down);
            valid_recursive_coords(Board, X1, Y1, X2, Y2, left);
            valid_recursive_coords(Board, X1, Y1, X2, Y2, up)
        ), Result).

%______________________________________________________________________________

% get all possible coords for a knight
% knight_coords(+Board, +X1, +Y1, -Result)
knight_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_knight_coords(Board, X1, Y1, X2, Y2), Result).

%______________________________________________________________________________

% get all possible coords for a bishop, where not empty
% bishop_coords(+Board, +X1, +Y1, -Result)
bishop_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, 
        (
            valid_recursive_coords(Board, X1, Y1, X2, Y2, up_right);
            valid_recursive_coords(Board, X1, Y1, X2, Y2, down_right);
            valid_recursive_coords(Board, X1, Y1, X2, Y2, down_left);
            valid_recursive_coords(Board, X1, Y1, X2, Y2, up_left)
        ), Result).

%______________________________________________________________________________

% get all possible coords for a queen
% queen_coords(+Board, +X1, +Y1, -Result)
queen_coords(Board, X1, Y1, Result):-
    bishop_coords(Board, X1, Y1, Result1),
    rook_coords(Board, X1, Y1, Result2),
    append(Result1, Result2, Result).

%_____________________________________________________

% get all possible coords for a piece of size N
% used by the game itself to get coords, like an API, sort of
% valid_coords(+Board, +X, +Y, +N, -Result)
valid_coords(Board, X, Y, 1, Result):-
    pawn_coords(Board, X, Y, Result).

valid_coords(Board, X, Y, 2, Result):-
    rook_coords(Board, X, Y, Result).

valid_coords(Board, X, Y, 3, Result):-
    knight_coords(Board, X, Y, Result).

valid_coords(Board, X, Y, 4, Result):-
    bishop_coords(Board, X, Y, Result).

valid_coords(Board, X, Y, 5, Result):-
    queen_coords(Board, X, Y, Result).

%______________________________________________________________________________

% Predicate to normalise piece, after it has been split. It makes sure the order of the disks inside the piece ae correct.
% normalise_piece(+Piece, -NormPiece, +Index)
normalise_piece(Piece, NormPiece, Index):-
    nth0(0, Piece, Size),
    Index =< Size,
    nth0(Index, Piece, Element),
    (number(Element) -> 
        Aux is Index, replace_row(Piece, Index, Aux, AuxPiece)
    ; 
        NewIndex is Index + 96,
        char_code(Char, NewIndex),
        replace_row(Piece, Index, Char, AuxPiece)

    ),
    NIndex is Index + 1,
    normalise_piece(AuxPiece, NormPiece, NIndex).
    
normalise_piece(Piece, Piece, Index):-
    nth0(0, Piece, Size),
    Index > Size.

%______________________________________________________________________________

% Predicate to split a piece into two pieces, given a number of disks to be moved
% split(+Piece, +N, -NewHead, -NewTail)    
split(Piece, N, NewHead, NewTail) :-
    length(NewTail, N),
    append(NewHead, NewTail, Piece).

%______________________________________________________________________________

% Predicate to normalise board, after a move has been made. It makes sure that empty squares are marked has empty
% normalise(+Old, -New)
normalise([0], empty).
normalise(Old,Old):- dif(Old, [0]).

%______________________________________________________________________________

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

    removehead(NewPiece, NewPiece1),
    T2 is X+N, %obter o tamanho da peça depois de mover

    NewPiece2 = [T2|NewPiece1], %colocar o tamanho da peça depois de mover
    
    append([NewPiece2, NNew], New), %colocar o tamanho da peça depois de mover

    normalise(Old, Old1), %normalizar a peça (passar par empty se for [0])
    normalise_piece(New, New1, 1), %normalizar a peça

    replace(Board, X1, Y1, Old1, NBoard), %colocar a peça na posição inicial
    replace(NBoard, X2, Y2, New1, NewBoard). %colocar a peça na posição final
