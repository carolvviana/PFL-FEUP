% Predicate to print the separator between lines of the matrix
% print_separator(+N)
print_separator(N):-
    write('  '),
    repeat_string('-+', N-1, Result),
    atom_concat('+', Result, TempResult),
    atom_concat(TempResult, '-+', FinalResult),
    write(FinalResult), !.

%_______________________________________________________

% Predicate to print the horizontal coordinates (X) of the matrix
% print_horizontal_coords(+N, +M)
print_horizontal_coords(N, M):-
    M < N,
    write(' '),
    write(M),
    M1 is M + 1,
    print_horizontal_coords(N, M1).

%_______________________________________________________

% Prints the matrix of the game
% p_m(+Matrix, +N)
p_m([_L|_T], N):-
    nl,
    write('  '),
    print_horizontal_coords(N, 0),
    nl.
    
p_m([L|T], N):-
    p_m([L|T], N, 0).

%_______________________________________________________

% Prints the matrix of the game with coordinates
% p_m(+Matrix, +N, +M)
p_m([], N, _):-
    nl,
    print_separator(N).

p_m([L|T], N, M):-
    nl,
    print_separator(N),
    nl,
    M1 is M + 1,
    write(M),
    write(' '),
    p_l(L), 
    write('|'),
    p_m(T, N, M1).

%_______________________________________________________

%Prints a line of the matrix and coords vertically
% p_l(+Line)
p_l([]).
p_l([C|L]):-
    write('|'), p_c(C),
    p_l(L).

%_______________________________________________________

% Prints the top piece of a tower in the matrix
% each cell is represented as [n,X], where n is the height of the tower and X is in the format [1,b,3,...]
% where each number/letter (1-6)/(a-f) represents a the height of the tower to the player with that number/letter.
% Numbers and letters are used to differentiate the colors of the pieces
% p_c(+Cell)
p_c(empty):-
    write(' '), !.

p_c([0]):-
    write(' '), !.

p_c(Piece):-
    last(Piece, TopPiece),
    write(TopPiece), !.


%_______________________________________________________

% Predicate to initialize the game state with size N
% initial_state(+Size, -GameState)
initial_state(N, List)  :- 
    length(AuxList, N),
    length(List, N), 
    maplist(=(empty), AuxList),
    maplist(=(AuxList), List).

%_______________________________________________________

% Predicate to display the game state
% display_game(+GameState) 
display_game(GameState):-
    length(GameState, Len),
    p_m(GameState, Len).

%_______________________________________________________
