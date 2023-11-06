:- consult('game.pl').
:- consult('ai.pl').
:- use_module(library(random)).

%_______________________________________________________________________________________________________________________

% Predicate to change the turn of the players
% 1 and 2 -> Player vs Player
% human-1 and ai-1 -> Player vs AI Level 1
% human-2 and ai-2 -> Player vs AI Level 2
% ai1-1 and ai2-1 -> AI Level 1 vs AI Level 1
% ai1-2 and ai2-2 -> AI Level 2 vs AI Level 2
% next_player(+Player, -NextPlayer)
next_player(1,2).
next_player(2,1).

next_player(human-1, ai-1).
next_player(ai-1, human-1).

next_player(human-2, ai-2).
next_player(ai-2, human-2).

next_player(ai1-1, ai2-1).
next_player(ai2-1, ai1-1).

next_player(ai1-2, ai2-2).
next_player(ai2-2, ai1-2).

%_______________________________________________________________________________________________________________________

% Predicate to display the players turn
% write_player_turn(+Player)
write_player_turn(2):-
    write('\n\n------ Player 2 turn ------ \n').

write_player_turn(1):-
    write('\n\n------ Player 1 turn ------ \n').

write_player_turn(ai-1):-
    write('\n\n------ AI turn ------ \n').

write_player_turn(human-1):-
    write('\n\n------ Human turn ------ \n').

write_player_turn(ai-2):-
    write('\n\n------ AI Level 2 turn ------ \n').

write_player_turn(human-2):-
    write('\n\n------ Human turn ------ \n').

write_player_turn(ai2-1):-
    write('\n\n------ AI Player 2 turn ------ \n').

write_player_turn(ai1-1):-
    write('\n\n------ AI Player 1 turn ------ \n').

write_player_turn(ai2-2):-
    write('\n\n------ AI Player 2 turn ------ \n').

write_player_turn(ai1-2):-
    write('\n\n------ AI Player 1 turn ------ \n').

%_______________________________________________________________________________________________________________________

% Congratulates the winner of the game
% congratulate(+Player)
congratulate(1):-
    write('\n----------- Player 1 won! Congratulations! -----------\n\n').

congratulate(2):-
    write('\n----------- Player 2 won! Congratulations! -----------\n\n').

congratulate(ai-1):-
    write('\n----------- AI Level 1 won! -----------\n\n').

congratulate(ai-2):-
    write('\n----------- AI Level 2 won! -----------\n\n').

congratulate(human-1):-
    write('\n----------- Human won! Congratulations! -----------\n\n').

congratulate(human-2):-
    write('\n----------- Human won! Congratulations! -----------\n\n').

congratulate(ai1-1):-
    write('\n----------- AI Player 1 won! -----------\n\n').

congratulate(ai2-1):-
    write('\n----------- AI Player 2 won! -----------\n\n').

congratulate(ai1-2):-
    write('\n----------- AI Player 1 won! -----------\n\n').

congratulate(ai2-2):-
    write('\n----------- AI Player 2 won! -----------\n\n').

%_______________________________________________________________________________________________________________________

% Predicate to check the history of the game (If a player's move cancels the other player's move, it is invalid)
% check_history(+History-+Index, +GameState, -NewHistory-+NewIndex)
check_history(History-Index, GameState, NewHistory-NewIndex):-
%trace,
    Previous is Index - 2,
    nth0(Previous, History, PreviousGameState),
    (PreviousGameState \= GameState
    ; (write('\nInvalid Move! Please choose another move'), fail)),
    append(History, [GameState], NewHistory),
    NewIndex is Index +1.

%_______________________________________________________________________________________________________________________


% Predicate that represents the Game Cycle
% game_cycle(+GameState-+Player, +History-+Index)
game_cycle(GameState-Player, _History-_Index):-
    game_over(GameState-Player, Winner), !,
    congratulate(Winner).

game_cycle(GameState-Player, History-Index):-
    repeat,
    move(GameState, Player, NewGameState),
    check_history(History-Index, NewGameState, NewHistory-NewIndex),
    write('\n------- CURRENT BOARD -------\n'),
    next_player(Player, NewPlayer),
    display_game(NewGameState), !,
    write_player_turn(NewPlayer),
    game_cycle(NewGameState-NewPlayer, NewHistory-NewIndex).

%_______________________________________________________________________________________________________________________

% Predicate to check if the game is over
% Checks if there is a piece with size 6
% game_over(+GameState, -Winner)
game_over(GameState-Player, Winner):-
    has_six_head(GameState), !,
    next_player(Player, Winner).

%_______________________________________________________________________________________________________________________

% Predicate that executes a move depending on the player
% move(+GameState, +Player, -NewGameState)
move(GameState, ai-2, NewGameState):-
    write('\n\nThinking.........\n'),
    choose_move(GameState, ai, 2,[Type | Move]),
    move_type(Type, Move, GameState, NewGameState).

move(GameState, ai1-2, NewGameState):-
    write('\n\nThinking.........\n'),
    choose_move(GameState, ai, 2,[Type | Move]),
    move_type(Type, Move, GameState, NewGameState).

move(GameState, ai2-2, NewGameState):-
    write('\n\nThinking.........\n'),
    choose_move(GameState, ai, 2,[Type | Move]),
    move_type(Type, Move, GameState, NewGameState).

move(GameState, Player, NewGameState):-
    choose_play(GameState, Option, Player),
    single_play(Option, GameState, Player, NewGameState).

%_______________________________________________________________________________________________________________________

% Predicate to get the type of play: Add a new piece or move a piece
% Option 1: Add a new piece
% Option 2: Move a piece
% choose_play(+GameState, -Option, +Player)
choose_play(GameState, Option, Player):-
    (Player = ai-1; Player = ai1-1; Player = ai2-1),
    findall(X-Y, (get_piece(GameState, X, Y, Piece), Piece \= empty), PieceCoords),
    PieceCoords \= [], %se tiver peças no tabuleiro
    write('\nWhat do you want to do?\n'),
    write('1). Add new piece.\n'),
    write('2). Move piece.\n'),
    random_select(Option, [1,2], _),
    write('Computer chose: Option '), write(Option), write('\n').

choose_play(GameState, Option, Player):-
    (Player = ai-1; Player = ai1-1; Player = ai2-1),
    findall(X-Y, (get_piece(GameState, X, Y, Piece), Piece \= empty), PieceCoords),
    PieceCoords = [], %se não tiver peças no tabuleiro
    write('\nWhat do you want to do?\n'),
    write('1). Add new piece.\n'),
    write('2). Move piece.\n'),
    Option = 1,
    write('Computer chose: Option '), write(Option), write('\n').

choose_play(_GameState, Option, Player):-
    (Player \= ai-1, Player \= ai1-1, Player \= ai2-1),
    repeat,
    get_play_input(Option),
    validate_play_input(Option), !.

%_______________________________________________________________________________________________________________________

% Predicate to do a single play, depending on the option chosen
% single_play(+Option, +GameState, +Player, -NewGameState)
single_play(1, Board, Player, NewBoard):-
    write('\n-----NEW PIECE----- \n'),
    new_piece_play(Board, Player, NewBoard).

single_play(2, Board, Player, NewBoard):-
    write('\n-----MOVE PIECE----- \n'),
    move_piece_play(Board, Player, NewBoard).

%_______________________________________________________________________________________________________________________

% Auxiliar predicate to show the options available and to read the user input
% get_play_input(-Option)
get_play_input(Option):-
    write('\nWhat do you want to do?\n'),
    write('1). Add new piece.\n'),
    write('2). Move piece.\n'),
    read(Option).

%_______________________________________________________________________________________________________________________

% Predicate to validate the user input
% validate_play_input(+Option)
validate_play_input(1).
validate_play_input(2).
validate_play_input(Option):-
    Option \= 1,
    Option \= 2,
    write('Invalid input. Please try again.\n'),
    fail.

%_______________________________________________________________________________________________________________________

% Predicate that allows player to choose where to add a new piece
% new_piece_play(+Board, +Player, -NewBoard)
new_piece_play(Board, ai1-1, NewBoard):-
    findall(X1-Y1, get_piece(Board, X1, Y1, empty), EmptyCoords),
    random_select(X-Y, EmptyCoords, _),
    add_new_piece(Board, X, Y, [1,1], NewBoard),
    write('\nNew Piece coordinates: ('), write(X), write(','), write(Y), write(')\n').

new_piece_play(Board, Player, NewBoard):-
    (Player = ai-1; Player = ai2-1),
    findall(X1-Y1, get_piece(Board, X1, Y1, empty), EmptyCoords),
    random_select(X-Y, EmptyCoords, _),
    add_new_piece(Board, X, Y, [1,a], NewBoard),
    write('\nNew Piece coordinates: ('), write(X), write(','), write(Y), write(')\n').

new_piece_play(Board, Player, NewBoard):-
    (Player \= ai-1, Player \= ai1-1, Player \= ai2-1),
    new_piece_where(Board, Len, _Piece, NewBoard, Player),
    length(NewBoard, Len).
    
%_______________________________________________________________________________________________________________________

% Predicate that read the input of the player regarding where he wants to add a new piece
% get_piece_input(-Piece)
get_piece_input(Piece):-
    write('Where do you want to add the piece? \n'),
    write('Please submit answer as (X,Y) \n'),
    read(Piece).

%_______________________________________________________________________________________________________________________    

% Predicate that receives were the player wants to add a new piece and adds it to the board if possible
% new_piece_where(+Board, -Len, -Piece, -NewBoard, +Player)
new_piece_where(Board, Len, Piece, NewBoard, Player) :-
    repeat,
    get_piece_input(Piece),
    length(Board, Len),
    validate_input(Board, Piece, Len, _X, _Y, NewBoard, Player), !.

%_______________________________________________________________________________________________________________________

% Predicate that validates input and add piece to board if possible
% validate_input(+Board,+Piece, +Len, -X, -Y, -NewBoard, +Player)
validate_input(_Board, Piece, Len, X, Y, _NewBoard, _Player):-
    \+validate_new(Piece, Len, X,Y),
    write('Invalid input. Please try again.\n'),
    fail.

validate_input(Board, Piece, Len, X, Y, NewBoard, _Player):-
    validate_new(Piece, Len, X,Y),

    \+add_new_piece(Board, X,Y, _, NewBoard),
    write('Board is not empty. Please choose an empty square.\n'),

    fail.

validate_input(Board, Piece, Len, X, Y, NewBoard, human-2):-
    validate_new(Piece, Len, X,Y),
    add_new_piece(Board, X,Y, [1,1], NewBoard).

validate_input(Board, Piece, Len, X, Y, NewBoard, human-1):-
    validate_new(Piece, Len, X,Y),
    add_new_piece(Board, X,Y, [1,1], NewBoard).

validate_input(Board, Piece, Len, X, Y, NewBoard, 1):-
    validate_new(Piece, Len, X,Y),
    add_new_piece(Board, X,Y, [1,1], NewBoard).

validate_input(Board, Piece, Len, X, Y, NewBoard, 2):-
    validate_new(Piece, Len, X,Y),
    add_new_piece(Board, X,Y, [1,a], NewBoard).

%_______________________________________________________________________________________________________________________

% Predicate that verifies if player chose coordinates in bounds of the board
% validate_new(+Piece, +Len, -X, -Y)
validate_new(Piece, Len, X,Y) :-
    Piece = (X,Y),
    X<Len,
    Y<Len.

%_______________________________________________________________________________________________________________________

% Predicate that allows player to choose where to move a piece
% move_piece_play(+Board, +Player, -NewBoard)
move_piece_play(Board, Player, NewBoard):-
    (Player = ai-1; Player = ai1-1; Player = ai2-1),

    findall(X-Y, (get_piece(Board, X, Y, Piece), Piece \= empty), PieceCoords), 
    random_select(X-Y, PieceCoords, _), %change piece where

    get_piece(Board, X, Y, Piece),
    nth0(0, Piece, N), %obter tamanho da peça
    NN is N + 1,

    random(1, NN, NDisks), %choose how many disks
    valid_coords(Board, X, Y, N, Result), %change piece to where

    random_select(X2-Y2, Result, _), %choose where to move
    move_piece(Board, X, Y, X2, Y2, NDisks, NewBoard),

    write('\nChose piece at coordinates: ('), write(X), write(','), write(Y), write(')\n'),
    write('\nMoved piece to coordinates: ('), write(X2), write(','), write(Y2), write(')\n').

move_piece_play(Board, Player, NewBoard):-
    (Player \= ai-1, Player \= ai1-1, Player \= ai2-1),

    change_piece_where(_Input, _Len, Board, Piece, X, Y), % which piece to move
    change_piece_to_where(Board, Piece, X, Y, Result), % possible moves for the piece
    print_coordinates(Result, 1),
    choose_where_to_move(Result, X2, Y2), % ask user where to move the piece
    choose_how_many_disks(NDisks, Piece), %how many disks to move
    move_piece(Board, X, Y, X2, Y2, NDisks, NewBoard), %move piece
    write('Great move!\n').

%_______________________________________________________________________________________________________________________

% Predicate that asks for input about which piece to move
% change_piece_input(-Piece)
change_piece_input(Piece):-
    write('Which piece do you want to change? \n'),
    write('Please submit answer as (X,Y) \n'),
    read(Piece).

%_______________________________________________________________________________________________________________________

% Predicate that allows player to choose which piece to move
% change_piece_where(-Input, -Len, +Board, -Piece, -X, -Y)
change_piece_where(Input, Len, Board, Piece, X, Y):-
    repeat,     
    change_piece_input(Input),

    length(Board, Len),

    validate_change(Input, Len, Board, Piece, X, Y), !.

validate_change(Input, Len, _Board, _Piece, X, Y):-
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
    Piece \= empty.

%_______________________________________________________________________________________________________________________

% Predicate that verifies if player chose coordinates that piece can move to, according to the rules
% change_piece_to_where(+Board, -Piece, +X, +Y, -Result)
change_piece_to_where(Board, Piece, X, Y, Result):-
    nth0(0, Piece, N), %obter tamanho da peça
    valid_coords(Board, X, Y, N, Result),
    dif(Result, []),
    write('Where do you want to move your piece?\n').

%_______________________________________________________________________________________________________________________

% Predicate to validate if user chose coordinates within the possible ones
%validate_coords(+Option, +Result)
validate_coords(Option, Result):-
    length(Result, Len),
    Option > Len,
    write('Invalid input. Please try again.\n'),

    fail.

validate_coords(Option, Result):-
    length(Result, Len),
    Option =< Len.

%_______________________________________________________________________________________________________________________

% Predicate that allows player to choose where to move a piece, from the possible coordinates
% choose_where_to_move(+Result, -X2, -Y2)
choose_where_to_move(Result, X2, Y2):-
    repeat,
    read(Option), % quais as coordenadas que a pessoa quer escolher
    validate_coords(Option, Result), !,

    nth1(Option, Result, NewCoords),
    NewCoords = X2-Y2.

choose_where_to_move(Result, _X2, _Y2):-
    repeat,
    read(Option), % quais as coordenadas que a pessoa quer escolher
    \+validate_coords(Option, Result), fail.

%_______________________________________________________________________________________________________________________

% Predicate that allows player to choose how many disks to move
% choose_how_many_disks(-NDisks, +Piece)
choose_how_many_disks(NDisks, Piece):-
    repeat,
    write('How many disks do you want to move?\n'),
    read(NDisks),

    validate_ndisks(NDisks, Piece),!.

%_______________________________________________________________________________________________________________________

% Predicate to validate if user chose a valid number of disks
% validate_ndisks(+NDisks, +Piece)
validate_ndisks(NDisks, Piece) :- % X e Y são as coordenadas da peça que a pessoa quer mover
    nth0(0, Piece, N), %obter tamanho da peça
    NDisks > N,
    write('Please choose a number of disks smaller or equal to the size of the piece.\n'),
    fail.

validate_ndisks(NDisks, Piece) :- % X e Y são as coordenadas da peça que a pessoa quer mover
    nth0(0, Piece, N), %obter tamanho da peça
    NDisks =< N,
    NDisks > 0.
