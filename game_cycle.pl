:- consult('game.pl').


change_player(1,2):-
    write('Player 2 turn \n').
    %play(). -> meter o jogador 2 a jogar

change_player(2,1):-
    write('Player 1 turn \n').

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

add_new_piece_input(Board, Player) :-
    write('Where do you want to add the piece? \n'),
    write('Please submit answer as (X,Y) \n'),

    read(Piece),
    length(Board, Len),

    %trace,
    validate_new(Piece, Len, Board, Player, NewBoard).

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

    change_piece(Result).

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



