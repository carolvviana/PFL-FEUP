:- consult('draw.pl').
:- consult('utils.pl').
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

%______________________________________________________________________________

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
% valid_rook_coords(_, X, Y, X, Y, _).
valid_recursive_coords(Board, X1, Y1, X2, Y2, Direction) :-
    length(Board, N),
    Size is N - 1,

    direction(Direction, DX, DY),

    between(1, Size, N1),
    X2 is X1 + DX*N1,
    Y2 is Y1 + DY*N1,

    get_piece(Board, X2, Y2, Piece),
    dif(Piece, empty), !.


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

% get all possible coords for a knight
% knight_coords(+Board, +X1, +Y1, -Result)
knight_coords(Board, X1, Y1, Result) :-
    findall(X2-Y2, valid_knight_coords(Board, X1, Y1, X2, Y2), Result).

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

% get all possible coords for a queen
% queen_coords(+Board, +X1, +Y1, -Result)
queen_coords(Board, X1, Y1, Result):-
    bishop_coords(Board, X1, Y1, Result1),
    rook_coords(Board, X1, Y1, Result2),
    append(Result1, Result2, Result).

%_____________________________________________________

% get all possible coords for a piece of size N
% used by the game itself to get coords, like an API, sort of
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

/*forall_custom(Lower, Upper, Goal) :-
    Lower =< Upper,
    call(Goal),
    NewLower is Lower + 1,
    forall_custom(NewLower, Upper, Goal).

forall_custom(Lower, Upper, _) :-
    Lower > Upper.


normalise_piece(Piece, NormPiece):-
    nth0(0, Piece, Size),
    nth0(0, NormPiece, Size),
    forall_custom(1, Size, (nth0(Index, Piece, Element),
        (number(Element) -> 
        Aux is Index, nth0(Index, NormPiece, Aux), write(Aux),nl
        ; 
        NewIndex is Index + 96,
        char_code(Char, NewIndex),
        nth0(Index, NormPiece, Char), write(Char),nl
        ))).


normalise_piece(Piece, NormPiece):-
    nth0(0, Piece, Size),
    nth0(0, NormPiece, Size),
    
    (between(1, Size, Index) -> 
        (nth0(Index, Piece, Element),
        (
            number(Element) -> 
                Aux is Index, nth0(Index, NormPiece, Aux)
                ; 
                NewIndex is Index + 96,
                char_code(Char, NewIndex),
                nth0(Index, NormPiece, Char)
        )
        )
    ).*/

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
    
split(Piece, N, NewHead, NewTail) :-
    length(NewTail, N),
    append(NewHead, NewTail, Piece).

normalise([0], empty).
normalise(Old,Old):- dif(Old, [0]).

% move N disks from piece at (X1,Y1) to piece at (X2,Y2)
% move_piece(+Board, +X1, +Y1, +X2, +Y2, +N, -NewBoard)
move_piece(Board, X1, Y1, X2, Y2, N, NewBoard) :-

    %trace,
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
    normalise_piece(New, New1, 1), %normalizar a peça

    replace(Board, X1, Y1, Old1, NBoard), %colocar a peça na posição inicial
    replace(NBoard, X2, Y2, New1, NewBoard). %colocar a peça na posição final


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
    [[x,o],  [x],    empty,     [pawn], [pawn]],
    [[o],    empty,  empty,     empty,  [pawn]],
    [empty,  [pawn], [x,x,o,o], [pawn], [pawn]],
    [[pawn], [pawn], empty,    [x],    [pawn]],
    [[pawn], [pawn], [pawn],    [pawn], [pawn]]
  ], 2, 2, 2, Result).

  1-3, 3-3, 0-0, 0-4

  valid_coords([[[x,o],  [x],    empty,     [pawn], [pawn]], [[o],    empty,  empty,     empty,  [pawn]], [empty,  [pawn], [x,x,o,o], [pawn], [pawn]], [[pawn], [pawn], [pawn],    [x],    [pawn]], [[pawn], [pawn], [pawn],    [pawn], [pawn]]], 2, 2, 4, Result).


valid_coords([
    [a,b,    c,    d,    e],
    [f,empty,empty,empty,j],
    [k,empty,m,    empty,o],
    [p,empty,empty,empty,t],
    [u,v,    w,    x,    y]
    ], 2, 2, 1, Result).

*/