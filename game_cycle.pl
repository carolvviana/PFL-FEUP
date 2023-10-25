:- consult('game.pl').


change_player(1,2):-
    write('Player 2 turn \n').
    %play(). -> meter o jogador 2 a jogar

change_player(2,1):-
    write('Player 1 turn \n').


encadeação_game():-
    new_piece_where(Board, Player, Len, Piece),
    validate_new(Piece, Len, Board, Player, NewBoard),


% Predicate that allows player to choose where to add a new piece
new_piece_where(Board, Player, Len, Piece) :-
    write('Where do you want to add the piece? \n'),
    write('Please submit answer as (X,Y) \n'),

    read(Piece),
    length(Board, Len).

    %validate_new(Piece, Len, Board, Player, NewBoard).

validate_new(Piece, Len, Board, Player, NewBoard) :-
    Piece = (X,Y),
    X<Len,
    Y<Len,
    \+add_new_piece(Board, X,Y, [1,1], NewBoard),
    write('Board is not empty. Please choose an empty square.\n'),
    add_new_piece_input(Board,1).

validate_new(Piece, Len, Board, Player, NewBoard) :-
    Piece \= (X,Y),
    write('Invalid input. Please try again.\n'),
    add_new_piece_input(Board,1).

validate_new(Piece, Len, Board, Player, NewBoard) :-
    Piece = (X,Y),
    X>=Len,
    write('Invalid input. Please try again.\n'),
    add_new_piece_input(Board,1).

validate_new(Piece, Len, Board, Player, NewBoard) :-
    Piece = (X,Y),
    Y>=Len,
    write('Invalid input. Please try again.\n'),
    add_new_piece_input(Board,1).

validate_new(Piece, Len, Board, 1, NewBoard) :-
    Piece = (X,Y),
    X<Len,
    Y<Len,
    add_new_piece(Board, X,Y, [1,1], NewBoard),
    p_m(NewBoard, Len),
    change_player(1,2).

validate_new(Piece, Len, Board, 2, NewBoard) :-
    Piece = (X,Y),
    X<Len,
    Y<Len,
    add_new_piece(Board, X,Y, [1,a], NewBoard),
    p_m(NewBoard, Len),
    change_player(2,1).


validate_change(Input, Len, Board, Player):-
    Input = (X,Y),
    X<Len,
    Y<Len,

    get_piece(Board, X, Y, Piece),
    Piece \= empty,

    write('Great choice!'),

    nth0(0, Piece, N), %obter tamanho da peça
    
    valid_coords(Board, X, Y, N, Result),

    write('Where do you want to move your piece? \n'),

    print_coordinates(Result, 1),

    change_piece(Result, N, X, Y, Board).

validate_change(Input, Len, Board, Player):-
    Input \= (X,Y),
    write('Invalid input. Please try again.\n'),
    change_piece_input(Board, Player).

validate_change(Input, Len, Board, Player):-
    Input = (X,Y),
    X>=Len,
    write('Invalid input. Please try again.\n'),
    change_piece_input(Board, Player).

validate_change(Input, Len, Board, Player):-
    Input = (X,Y),
    X<Len,
    Y>=Len,
    write('Invalid input. Please try again.\n'),
    change_piece_input(Board, Player).

change_piece_input(Board, Player):-
    write('Which piece do you want to change? \n'),
    write('Please submit answer as (X,Y) \n'),

    read(Piece),
    length(Board, Len),

    validate_change(Piece, Len, Board, Player).


change_piece(Result, N, X, Y, Board):- 

    read(Option), % quais as coordenadas que a pessoa quer escolher
    nth1(Option, Result, NewCoords),

    NewCoords = X2-Y2,

    write('How many disks do you want to move?\n'),
    read(NDisks),

    validate_ndisks(X, Y, X2, Y2, NDisks, N, Board).

validate_ndisks(X, Y, X2, Y2, NDisks, N, Board) :-
    NDisks =< N,
    NDisks > 0,
    
    write('Great choice!\n'),
    move_piece(Board, X, Y, X2, Y2, NDisks, NewBoard),
    lenght(NewBoard, Len),
    p_m(NewBoard, Len).
    % fazer algo

validate_ndisks(X, Y, X2, Y2, NDisks, N, Board) :-
    NDisks < 0,
    write('Please choose a positive number of disks.\n'),
    change_piece(Result, N).
    
validate_ndisks(X, Y, X2, Y2, NDisks, N, Board) :-
    NDisks > N,
    write('Please choose a number of disks smaller or equal to the size of the piece.\n'),
    change_piece(Result, N).

