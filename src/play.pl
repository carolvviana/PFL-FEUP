:- consult('game_cycle.pl').

%_________________________________________

% Valid difficulty options for the second menu
valid_difficulty_input(1).
valid_difficulty_input(2).

%_________________________________________

% Valid board size options for the third menu
valid_board_size_input(N) :- N >= 2.

%_________________________________________

% Predicates that represent valid menu options 
first_menu_input(1) :-
    play_menu.

first_menu_input(2) :-
    write('\nSee you later!\n').

%_________________________________________

% Predicates that represent valid menu options 
% Player vs. Player
second_menu_input(1) :-
   board_size_menu(1).

% Player vs. AI
second_menu_input(2) :-
    write('Choose the difficulty: \n'),
    write('\n1. Easy\n'),
    write('2. Hard\n'),

    write('\nPlease enter your choice: \n'),

    repeat,
    read(Input),

    valid_difficulty_input(Input),
    Answer is 1 + Input,
    board_size_menu(Answer).

% AI vs. AI
second_menu_input(3) :-
    write('Choose the difficulty: \n'),
    write('\n1. Easy\n'),
    write('2. Hard\n'),

    write('\nPlease enter your choice: \n'),

    repeat,
    read(Input),

    valid_difficulty_input(Input),
    Answer is 3 + Input,
    board_size_menu(Answer).

second_menu_input(4) :-
    play.

%_________________________________________

% Predicate that represents valid menu options for the first menu
display_menu :-
    write('\nSIX-MAKING\n'),
    write('\n1. Play Game\n'),
    write('2. Exit\n'),

    write('\nPlease enter your choice: \n'),

    %read(Input),
    repeat,
    read(Input),
    first_menu_input(Input).

%_________________________________________

% Predicate that represents the valid menu options for the second menu - Select game mode
play_menu :-
    write('\n1. Player vs. Player\n'),
    write('2. Player vs. AI\n'),
    write('3. AI vs. AI\n'),
    write('4. Back\n'),

    write('\nPlease enter your choice: \n'),

    repeat,
    read(Input),

    second_menu_input(Input).

%_________________________________________

% Predicate that represents the valid menu options for the third menu - Select board size
% TypeOfGame = 1 -> Player vs. Player
% TypeOfGame = 2 -> Player vs. AI -> easy
% TypeOfGame = 3 -> Player vs AI -> hard
% TypeOfGame = 4 -> AI vs. AI -> easy
% TypeOfGame = 5 -> AI vs AI -> hard
board_size_menu(TypeOfGame) :-
    write('\nChoose the board size: (N)\n'),

    repeat, 
    read(Input),

    % Input -> Size of the board
    valid_board_size_input(Input),
    game_mode(TypeOfGame, Input).

%_________________________________________

% Predicate that generates initial empty history
initial_history([[],[]]-2).

% Predicate that represents the valid menu options for the fourth menu - Select difficulty
game_mode(1, SizeOfBoard) :- 
    initial_state(SizeOfBoard, InitialGameState),
    initial_history(InitialHistory-Index),
    write('\n\n----------------------LET\'S BEGIN--------------------\n\n'),
    write('                     Initial Board                     \n'),
    p_m(InitialGameState, SizeOfBoard), nl,
    game_cycle(InitialGameState-1, InitialHistory-Index).

game_mode(2, SizeOfBoard) :- 
    initial_state(SizeOfBoard, InitialGameState),
    initial_history(InitialHistory-Index),
    write('\n\n----------------------LET\'S BEGIN--------------------\n\n'),
    write('                     Initial Board                     \n'),
    p_m(InitialGameState, SizeOfBoard), nl,
    game_cycle(InitialGameState-(human-1), InitialHistory-Index).

game_mode(3, SizeOfBoard) :- 
    initial_state(SizeOfBoard, InitialGameState),
    initial_history(InitialHistory-Index),
    write('\n\n----------------------LET\'S BEGIN--------------------\n\n'),
    write('                     Initial Board                     \n'),
    p_m(InitialGameState, SizeOfBoard), nl,
    game_cycle(InitialGameState-(human-2), InitialHistory-Index).

game_mode(4, SizeOfBoard) :- 
    initial_state(SizeOfBoard, InitialGameState),
    initial_history(InitialHistory-Index),
    write('\n\n----------------------LET\'S BEGIN--------------------\n\n'),
    write('                     Initial Board                     \n'),
    p_m(InitialGameState, SizeOfBoard), nl,
    game_cycle(InitialGameState-(ai1-1), InitialHistory-Index).

game_mode(5, SizeOfBoard) :- 
    initial_state(SizeOfBoard, InitialGameState),
    initial_history(InitialHistory-Index),
    write('\n\n----------------------LET\'S BEGIN--------------------\n\n'),
    write('                     Initial Board                     \n'),
    p_m(InitialGameState, SizeOfBoard), nl,
    game_cycle(InitialGameState-(ai1-2), InitialHistory-Index).

