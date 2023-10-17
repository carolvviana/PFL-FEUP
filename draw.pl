%print the matrix of the game
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

%prints a line of the matrix
%TODO: change to have a better representation
p_l([]).
p_l([C|L]):-
    write('|'), p_c(C),
    p_l(L).

p_c(_-X):-
    write(X), !.

p_c(empty):-
    write(' '), !.

% |2|1|
% --+--
% 