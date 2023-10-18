repeat_string(_, 0, '').
repeat_string(Str, N, Result):-
    N > 0,
    NewN is N - 1,
    repeat_string(Str, NewN, NewResult),
    atom_concat(Str, NewResult, Result).

print_separator(N):-
    repeat_string('-+', N-1, Result),
    atom_concat('+', Result, TempResult),
    atom_concat(TempResult, '-+', FinalResult),
    write(FinalResult), !.

% prints the coords of the matrix
% print_coords_h(+N)
print_coords_h(N):-
    write(' '),
    print_coords_h_aux(N, 1).


p_b([],N):- nl.
p_b([H|T], N):-
    nl,
    print_coords_h(N),
    p_m([H|T], N).
    
% prints the matrix of the game with coords
% p_m(+Matrix, +N)
p_m([], N):-
    nl,
    print_separator(N).

p_m([L|T], N):-
    nl,
    print_separator(N),
    nl,
    p_l(L), 
    write('|'),
    p_m(T, N).

%_______________________________________________________

%prints a line of the matrix and coords vertically
% p_l(+Line)
p_l([]).
p_l([C|L]):-
    write('|'), p_c(C),
    p_l(L).

%_______________________________________________________

% prints the top piece of a tower in the matrix
% each cell is represented as n-X, where n is the height of the tower and X is in the format 1-b-3-...
% where each number/letter (1-6)/(a-f) represents a the height of the tower to the player with that number/letter.
% Numbers and letters are used to differentiate the colors of the pieces
% p_c(+Cell)
p_c(_-X):-
    write(X), !.

p_c(empty):-
    write(' '), !.

%_______________________________________________________

