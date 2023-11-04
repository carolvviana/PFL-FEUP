:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('game.pl').

% evaluate the board, used for minimax algorithm
% value(+Board, -Value)
value(Board, Value):-
    findall(Piece, (member(Row, Board), member(Piece, Row), Piece \= empty), Pieces),
    length(Pieces, NPieces), % number of pieces on the board
    largest_piece(Pieces, Largest), % size of the largest piece on the board
    findall(Size, (member(Piece, Pieces), piece_size(Piece, Size)), Sizes),
    sumlist(Sizes, Sum), % sum of the sizes of all pieces on the board

    Value is 0.6 * Largest + 0.3 * (Sum/NPieces) + 0.1 * NPieces.

    %length(Board, NRows),
    %p_m(Board, NRows),
    %write('\nValue: '), write(Value), nl.

% move(+GameState, +Move, -NewGameState).
move(Board, [Type | Move], NewBoard) :-
    move_type(Type, Move, Board, NewBoard).

move_type(place, [Piece, X, Y], Board, NewBoard) :-
    add_new_piece(Board, X, Y, Piece, NewBoard).

move_type(move, [N, X-Y, NewX-NewY], Board, NewBoard) :-
    move_piece(Board, X, Y, NewX, NewY, N, NewBoard).

% minimax(+GameState, +Player, +Type, +Depth, -Value)
% Minimax algorithm with depth 2
minimax(_, _, 2, 0):- !.
minimax(GameState, Type, Depth, Value):-
	swap_minimax(Type, NewType),
    NextDepth is Depth + 1,
	valid_moves(GameState, ListOfMoves),
	setof(Val,
        (
            member(Move, ListOfMoves), 
            move(GameState, Move, NewGameState), 
            value(NewGameState, Value1),
            minimax(NewGameState, NewType, NextDepth, Value2), 
            Val is Value1 + Value2
        ),
        Values),
    eval(Type, Values, Value).

swap_minimax(max, min).
swap_minimax(min, max).

% eval(+MiniMaxMode, +Values, -Result)
% Unifies Result with the value according to the MiniMax mode
eval(min, [Value|_], Result):- Result is -Value.
eval(max, Values, Value):- last(Values, Value).

% choose_move(+GameState,+Player,+Level,-Move)
% Bot greedy player. Makes a list of possible moves and select the one with the most points according minimax algorithm
choose_move(GameState, ai-2, Move):-
%trace,
	valid_moves(GameState, ListOfMoves),
    findall(
        Value-Move,
        ( 
            member(Move, ListOfMoves), 
            move(GameState, Move, NewGameState), 
            value(NewGameState, Value1),
            minimax(NewGameState, max, 1, Value2),
            Value is Value1 + Value2
        ),
        Pairs
        ),
    sort(Pairs, SortedPairs),
    last(SortedPairs, Max-_),
    findall(Moves, member(Max-Moves, SortedPairs), MaxMoves),
    random_member(Move, MaxMoves).

% get all possible moves, either placing a new piece or moving an existing one
% each move is a list of the form [Type, X-Y, NewX-NewY]
% Type is either place or move
% X-Y is the coordinates of the piece to be placed or moved
% NewX-NewY is the coordinates of the new position of the piece in case of a move only
% valid_moves(+Board, -ListOfMoves)
valid_moves(Board, ListOfMoves):-
    findall([move, N, X-Y, NewX-NewY], (get_piece(Board, X, Y, [Size | Piece]), [Size | Piece] \= empty, valid_coords(Board, X, Y, Size, Coords), member(NewX-NewY, Coords), between(1, Size, N)), ListOfMoves2),
    findall([place, [1,a], X, Y], get_piece(Board, X, Y, empty), ListOfMoves1),
    append(ListOfMoves1, ListOfMoves2, ListOfMoves).
    %ListOfMoves2 = ListOfMoves.