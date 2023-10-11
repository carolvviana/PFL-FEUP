%print the matrix of the game
p_m([]):- nl, write('--+-+--').
p_m([L|T]):-
    nl,
    write('--+-+--'),
    nl,
    p_l(L), 
    write('|'),
    p_m(T).

%prints a line of the matrix
%TODO: change to have a better representation
p_l([]).
p_l([C|L]):-
    write('|'), p_c(C),
    p_l(L).

%prints a character of the matrix
% p_c(_-_-_-_-_-_-X):-
%     write(X), !.

% p_c(_-_-_-_-_-X):-
%     write(X), !.

% p_c(_-_-_-_-X):-
%     write(X), !.

% p_c(_-_-_-X):-
%     write(X), !.

% p_c(_-_-X):-
%     write(X), !.

p_c(_-X):-
    write(X), !.

p_c(empty):-
    write(' '), !.

_ -> 1-1-a-X

%     t(C,S),
%     wi

% p_c(C):-
%     t(C,S),
%     write(S).

% t():-


% 1-A-2
% p_c()