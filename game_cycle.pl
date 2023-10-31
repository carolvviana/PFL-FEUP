:- consult('game.pl').


change_player(1,2):-
    write('Player 2 turn \n').
    %play(). -> meter o jogador 2 a jogar

change_player(2,1):-
    write('Player 1 turn \n').

%_______________________________________________________________________________________________________________________

% Predicate that allows player to choose where to add a new piece
% encadeação_game(+Board, +Player, -NewBoard)
encadeacao_game(Board, Player, NewBoard):-

    new_piece_where(Board, Len, _Piece, NewBoard, Player),
    length(NewBoard, Len),
    p_m(NewBoard, Len).

%_______________________________________________________________________________________________________________________

% Predicate that read the input of the player regarding where he wants to add a new piece
% get_piece_input(-Piece)
get_piece_input(Piece):-
    write('Where do you want to add the piece? \n'),
    write('Please submit answer as (X,Y) \n'),
    read(Piece).

%_______________________________________________________________________________________________________________________    

% Predicate that receives were player wants to add a new piece and adds it to the board if possible
% new_piece_where(+Board, -Len, -Piece, -NewBoard, +Player)
new_piece_where(Board, Len, Piece, NewBoard, Player) :-
    repeat,
    get_piece_input(Piece),

    length(Board, Len),

    validate_input(Board, Piece, Len, X, Y, NewBoard, Player), !.

new_piece_where(Board, Len, Piece, NewBoard, Player) :-
    repeat,
    get_piece_input(Piece),

    length(Board, Len),

    \+validate_input(Board, Piece, Len, X, Y, NewBoard, Player), fail.

%_______________________________________________________________________________________________________________________

% Predicate that validates input and add piece to board if possible
% validate_input(+Board,+Piece, +Len, -X, -Y, -NewBoard, +Player)
validate_input(Board, Piece, Len, X, Y, NewBoard, Player):-
    \+validate_new(Piece, Len, X,Y),
    write('Invalid input. Please try again.\n'),

    fail.

validate_input(Board, Piece, Len, X, Y, NewBoard, Player):-
    validate_new(Piece, Len, X,Y),

    \+add_new_piece(Board, X,Y, _, NewBoard),
    write('Board is not empty. Please choose an empty square.\n'),

    fail.

validate_input(Board, Piece, Len, X, Y, NewBoard, 1):-
    validate_new(Piece, Len, X,Y),
    add_new_piece(Board, X,Y, [1,1], NewBoard),

    write('Great choice!').

validate_input(Board, Piece, Len, X, Y, NewBoard, 2):-
    validate_new(Piece, Len, X,Y),
    add_new_piece(Board, X,Y, [1,a], NewBoard),

    write('Great choice!').

%_______________________________________________________________________________________________________________________

% Predicate that verifies if player chose coordinates in bounds of the board
% validate_new(+Piece, +Len, -X, -Y)
validate_new(Piece, Len, X,Y) :-
    Piece = (X,Y),
    X<Len,
    Y<Len.

%_______________________________________________________________________________________________________________________


encadeacao(Board, Player):-
%trace,

    change_piece_where(Input, Len, Board, Piece, X, Y),

    change_piece_to_where(Board, Piece, X, Y, Result),

    print_coordinates(Result, 1),
    %trace,

    choose_where_to_move(Result, X2, Y2),

    choose_how_many_disks(NDisks, Piece),

    move_piece(Board, X, Y, X2, Y2, NDisks, NewBoard),

    write('Great move!\n'),

    p_m(NewBoard, Len).






change_piece_input(Piece):-
    write('Which piece do you want to change? \n'),
    write('Please submit answer as (X,Y) \n'),

    read(Piece).

    
change_piece_where(Input, Len, Board, Piece, X, Y):-
    repeat,     
    change_piece_input(Input),

    length(Board, Len),

    validate_change(Input, Len, Board, Piece, X, Y), !.

change_piece_where(Input, Len, Board, Piece, X, Y):-
    repeat,     
    change_piece_input(Input),

    length(Board, Len),

    \+validate_change(Input, Len, Board, Piece, X, Y), fail.


validate_change(Input, Len, Board, Piece, X, Y):-
    \+validate_new(Input, Len, X, Y),
    write('Invalid input. Please try again.\n'),
    %change_piece_where(_, _, Board, _, _, _),
    fail.

validate_change(Input, Len, Board, Piece, X, Y):-
    validate_new(Input, Len, X, Y),

    get_piece(Board, X, Y, Piece),
    Piece = empty,
    write('Invalid input. Please try again.\n'),
    %change_piece_where(_, _, Board, _, _, _),
    fail.

validate_change(Input, Len, Board, Piece, X, Y):-
    validate_new(Input, Len, X, Y),

    get_piece(Board, X, Y, Piece),
    Piece \= empty,

    write('Great choice!\n').



change_piece_to_where(Board, Piece, X, Y, Result):-
    nth0(0, Piece, N), %obter tamanho da peça
    valid_coords(Board, X, Y, N, Result),
    write('Where do you want to move your piece?\n').



validate_coords(Option, Result):-
    length(Result, Len),
    Option > Len,
    write('Invalid input. Please try again.\n'),

    %choose_where_to_move(Result, _,_).
    fail.

validate_coords(Option, Result):-
    length(Result, Len),
    Option =< Len,
    write('Great choice!\n').


choose_where_to_move(Result, X2, Y2):-
    repeat,
    read(Option), % quais as coordenadas que a pessoa quer escolher
    validate_coords(Option, Result), !,

    nth1(Option, Result, NewCoords),
    NewCoords = X2-Y2.

choose_where_to_move(Result, X2, Y2):-
    repeat,
    read(Option), % quais as coordenadas que a pessoa quer escolher
    \+validate_coords(Option, Result), fail.


choose_how_many_disks(NDisks, Piece):-
    repeat,
    write('How many disks do you want to move?\n'),
    read(NDisks),

    validate_ndisks(NDisks, Piece),!.

choose_how_many_disks(NDisks, Piece, Ret):-
    repeat,
    write('How many disks do you want to move?\n'),
    read(NDisks),

    \+validate_ndisks(NDisks, Piece),
    fail.



validate_ndisks(NDisks, Piece) :- % X e Y são as coordenadas da peça que a pessoa quer mover
    nth0(0, Piece, N), %obter tamanho da peça
    NDisks > N,
    write('Please choose a number of disks smaller or equal to the size of the piece.\n'),
    fail.

validate_ndisks(NDisks, Piece) :- % X e Y são as coordenadas da peça que a pessoa quer mover
    nth0(0, Piece, N), %obter tamanho da peça
    NDisks =< N,
    NDisks > 0,
    write('Great choice!\n').


/*

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

/*validate_change(Input, Len, Board, Player):-
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
    change_piece_input(Board, Player).*/

/*


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

*/