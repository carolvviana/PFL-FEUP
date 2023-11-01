:- consult('game.pl').

% Valid menu options for the first menu

valid_first_menu_input(1).
valid_first_menu_input(2).
valid_first_menu_input(3).

%_________________________________________

% Valid menu options for the second menu

valid_second_menu_input(1).
valid_second_menu_input(2).
valid_second_menu_input(3).
valid_second_menu_input(4).
%valid_second_menu_input(N) :- fail.


%_________________________________________

% Valid difficulty options for the second menu
valid_difficulty_input(1).
valid_difficulty_input(2).

%_________________________________________

% Valid board size options for the third menu
valid_board_size_input(N) :- N >= 2.

% Predicates that represent valid menu options 
first_menu_input(1) :-
    write('\ninput 1\n'),
    play_menu.

first_menu_input(2) :-
    write('\n rules'), nl.

first_menu_input(3) :-
    write('\nSee you later!\n').

%_________________________________________

% Predicates that represent valid menu options 
% Player vs. Player
second_menu_input(1) :-
   write('\ninput 1\n'),
   board_size_menu(1).

% Player vs. AI
second_menu_input(2) :-
    write('\ninput 2\n'),
    nl,
    write('Choose the difficulty: \n'),
    write('\n1. Easy\n'),
    write('2. Hard\n'),

    write('\nPlease enter your choice: \n'),

    get_char(UserInput), skip_line,
    char_code(UserInput, Char),
    Input is Char - 48,

    valid_difficulty_input(Input),
    Answer is 1 + Input,
    board_size_menu(Answer).


% AI vs. AI
second_menu_input(3) :-
    write('\ninput 3\n'),

    nl,
    write('Choose the difficulty: \n'),
    write('\n1. Easy\n'),
    write('2. Hard\n'),

    write('\nPlease enter your choice: \n'),

    get_char(UserInput), skip_line,
    char_code(UserInput, Char),
    Input is Char - 48,

    valid_difficulty_input(Input),
    Answer is 3 + Input,
    board_size_menu(Answer).
    %write('\nSee you later!\n').

second_menu_input(4) :-
    play.

%_________________________________________

% Predicate that represents valid menu options for the first menu
play :-
    write('\nSIX-MAKING\n'),
    write('\n1. Play Game\n'),
    write('2. Rules\n'),
    write('3. Exit\n'),

    write('\nPlease enter your choice: \n'),

    %read(Input),
    get_char(UserInput), skip_line,
    char_code(UserInput, Char),
    Input is Char - 48,
    write(Input), nl,
    valid_first_menu_input(Input),
    first_menu_input(Input).

%_________________________________________

% Predicate that represents the valid menu options for the second menu - Select game mode

play_menu :-
    write('\n1. Player vs. Player\n'),
    write('2. Player vs. AI\n'),
    write('3. AI vs. AI\n'),
    write('4. Back\n'),

    write('\nPlease enter your choice: \n'),

    %read(Input),
    get_char(UserInput), skip_line,
    char_code(UserInput, Char),
    Input is Char - 48,
    valid_second_menu_input(Input),
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

    get_char(UserInput), skip_line,
    char_code(UserInput, Char),
    Input is Char - 48,
    % Input -> Size of the board
    valid_board_size_input(Input),
    game_mode(TypeOfGame, Input).

%_________________________________________

% Predicate that represents the valid menu options for the fourth menu - Select difficulty
game_mode(1, SizeOfBoard) :- write('Player vs. Player').
game_mode(2, SizeOfBoard) :- write('Player vs. AI -> easy').
game_mode(3, SizeOfBoard) :- write('Player vs. AI -> hard').
game_mode(4, SizeOfBoard) :- write('AI vs. AI -> easy').
game_mode(5, SizeOfBoard) :- write('AI vs. AI -> hard').
    

play_game:-
    initial_state(GameState-Player),
    display_game(GameState-Player),
    game_cycle(GameState-Player).

game_cycle(GameState-Player):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState-Player):-
    choose_move(GameState, Player, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState-NextPlayer), !,
    game_cycle(NewGameState-NextPlayer).

next_player(1, 2).   
next_player(2, 1).
next_player(human, computer).
next_player(computer, human).


choose_move(GameState, human, Move):-
% interaction to select move

choose_move(GameState, computer-Level, Move):-
    valid_moves(GameState, Moves),
    choose_move(Level, GameState, Moves, Move).

choose_move(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).

valid_moves(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves).

choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
    move(GameState, Mv, NewState),
    evaluate_board(NewState, Value) ), [_V-Move|_]).
% evaluate_board assumes lower value is better
