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

%_________________________________________

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
second_menu_input(1) :-
   write('\ninput 1\n').
   % play_menu.

second_menu_input(2) :-
    write('\ninput 2\n').

second_menu_input(3) :-
    write('\ninput 3\n').
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