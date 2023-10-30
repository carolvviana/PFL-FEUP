repeat_string(_, 0, '').
repeat_string(Str, N, Result):-
    N > 0,
    NewN is N - 1,
    repeat_string(Str, NewN, NewResult),
    atom_concat(Str, NewResult, Result).

print_separator(N):-
    write('  '),
    repeat_string('-+', N-1, Result),
    atom_concat('+', Result, TempResult),
    atom_concat(TempResult, '-+', FinalResult),
    write(FinalResult), !.


print_horizontal_coords(N, M):-
    M < N,
    write(' '),
    write(M),
    M1 is M + 1,
    print_horizontal_coords(N, M1).


% prints the matrix of the game
% p_m(+Matrix, +N)
p_m([L|T], N):-
    nl,
    write('  '),
    print_horizontal_coords(N, 0),
    nl.
    
p_m([L|T], N):-
    p_m([L|T], N, 0).
% prints the matrix of the game with coords
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
p_c(empty):-
    write(' '), !.

p_c([0]):-
    write(' '), !.

p_c(Piece):-
    last(Piece, TopPiece),
    write(TopPiece), !.


%_______________________________________________________

print_coordinates([],_).
print_coordinates([X-Y|T], N) :-
    %trace,
    N1 is N+1,
    write(N), write('). '), write(X), write(','), write(Y), write('\n'),
    print_coordinates(T, N1).
