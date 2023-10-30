:- consult('draw.pl').
:- consult('utils.pl').
:- use_module(library(lists)).
:- use_module(library(between)).

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
% valid_rook_coords(_, X, Y, X, Y, _).
valid_rook_coords(Board, X1, Y1, X2, Y2, Direction) :-
    length(Board, N),
    Size is N - 1,

    (
        Direction = up, Y is Y1 - 1, NewX is X1, reverse_between(Y, 0, NewY);
        Direction = down, Y is Y1 + 1, NewX is X1, between(Y, Size, NewY);
        Direction = left, X is X1 - 1, NewY is Y1, reverse_between(X, 0, NewX);
        Direction = right, X is X1 + 1, NewY is Y1, between(X, Size, NewX)
    ),
    
    get_piece(Board, NewX, NewY, Piece),
    dif(Piece, empty), !,
    Y2 is NewY,
    X2 is NewX.


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

% get all valid coords for a bishop, where not empty
% valid_bishop_coords(+Board, +X1, +Y1, ?X2, ?Y2, ?Direction)
valid_bishop_coords(Board, X1, Y1, X2, Y2, Direction) :-
    length(Board, N),
    Size is N - 1,

    (
        Direction = up_right, X is X1 + 1, Y is Y1 - 1, between(X, Size, NewX), reverse_between(Y, 0, NewY);
        Direction = down_right, X is X1 + 1, Y is Y1 + 1, between(X, Size, NewX), between(Y, Size, NewY);
        Direction = down_left, X is X1 - 1, Y is Y1 + 1, reverse_between(X, 0, NewX), between(Y, Size, NewY);
        Direction = up_left, X is X1 - 1, Y is Y1 - 1, reverse_between(X, 0, NewX), reverse_between(Y, 0, NewY)
    ),
    
    get_piece(Board, NewX, NewY, Piece),
    dif(Piece, empty), !,
    Y2 is NewY,
    X2 is NewX.

%______________________________________________________________________________

% get all possible coords for a pawn
%pawn_coords(+Board, +X1, +Y1, -Result)
pawn_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_pawn_coords(Board, X1, Y1, X2, Y2), Result).

% get all possible coords for a rook, where not empty
% rook_coords(+Board, +X1, +Y1, -Result)
rook_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, 
        (
            valid_rook_coords(Board, X1, Y1, X2, Y2, right);
            valid_rook_coords(Board, X1, Y1, X2, Y2, down);
            valid_rook_coords(Board, X1, Y1, X2, Y2, left);
            valid_rook_coords(Board, X1, Y1, X2, Y2, up)
        ), Result).

% get all possible coords for a knight
% knight_coords(+Board, +X1, +Y1, -Result)
knight_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_knight_coords(Board, X1, Y1, X2, Y2), Result).

% get all possible coords for a bishop, where not empty
% bishop_coords(+Board, +X1, +Y1, -Result)
bishop_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, 
        (
            valid_bishop_coords(Board, X1, Y1, X2, Y2, up_right);
            valid_bishop_coords(Board, X1, Y1, X2, Y2, down_right);
            valid_bishop_coords(Board, X1, Y1, X2, Y2, down_left);
            valid_bishop_coords(Board, X1, Y1, X2, Y2, up_left)
        ), Result).

% get all possible coords for a queen
% queen_coords(+Board, +X1, +Y1, -Result)
queen_coords(Board, X1, Y1, Result):-
    bishop_coords(Board, X1, Y1, Result1),
    rook_coords(Board, X1, Y1, Result2),
    append(Result1, Result2, Result).

%_____________________________________________________

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



%tests
/*

valid_coords([
    [[1,a],empty,[1,c],[1,d],[1,e]],
    [[1,f],[2-g-z],[1,h],[1,i],[1,j]],
    [[1,k],empty,[1,m],[1,n],[1,o]],
    [[1,p],[1,q],[1,r],[1,s],[1,t]],
    [[1,u],[1,v],[1,w],[1,x],[1,y]]
    ], 2, 2, 1, Result).

valid_coords([
    [a,b,    c,    d,    e],
    [f,empty,empty,empty,j],
    [k,empty,m,    empty,o],
    [p,empty,empty,empty,t],
    [u,v,    w,    x,    y]
    ], 2, 2, 1, Result).

*/