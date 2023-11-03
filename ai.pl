:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('game.pl').

evaluate_board(Board, Value):-
    findall(Piece, (member(Row, Board), member(Piece, Row), Piece \= empty), Pieces),
    length(Pieces, NPieces), % number of pieces on the board
    largest_piece(Pieces, Largest), % size of the largest piece on the board
    findall(Size, (member(Piece, Pieces), piece_size(Piece, Size)), Sizes),
    sumlist(Sizes, Sum), % sum of the sizes of all pieces on the board

    write('Largest: '), write(Largest), nl,
    write('Mean: '), write(Sum/NPieces), nl,
    write('NPieces: '), write(NPieces), nl,

    Value is 0.5 * Largest + 0.3 * (Sum/NPieces) + 0.2 * NPieces.

largest_piece(Pieces, Largest) :-
    maplist(piece_size, Pieces, Sizes),
    list_max(Sizes, Largest).

simulate_move([Type | Move], Board, NewBoard) :-
    move_type(Type, Move, Board, NewBoard).

move_type(place, [Piece, Row, Col], Board, NewBoard) :-
    place_piece(Board, Piece, Row, Col, NewBoard).

move_type(move, [Row, Col, NewRow, NewCol], Board, NewBoard) :-
    move_piece(Board, Row, Col, NewRow, NewCol, NewBoard).

% minimax(+GameState, +Player, +Type, +Depth, -Value)
% Minimax algorithm with depth 2
minimax(_, _, 2, 0):- !.
minimax(GameState, Type, Depth, Value):-
	swap_minimax(Type, NewType),
    NextDepth is Depth + 1,
	get_all_moves(GameState, ListOfMoves),
	setof(Val,
        (
            member(Move, ListOfMoves), 
            simulate_move(Move, GameState, NewGameState), 
            evaluate_board(NewGameState, Value1),
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
	get_all_moves(GameState, ListOfMoves),
	findall(
        Value-Move,
        ( 
            member(Move, ListOfMoves), 
            simulate_move(Move, GameState, NewGameState), 
            evaluate_board(NewGameState, Value1),
            minimax(NewGameState, min, 1, Value2),
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
% get_all_moves(+Board, -ListOfMoves)
get_all_moves(Board, ListOfMoves):-
    findall([move, X-Y, NewX-NewY], (get_piece(Board, X, Y, [Size | Piece]), [Size | Piece] \= empty, valid_coords(Board, X, Y, Size, Coords), member(NewX-NewY, Coords)), ListOfMoves2),
    findall([place, [1,a], X, Y], get_piece(Board, X, Y, empty), ListOfMoves1),
    append(ListOfMoves1, ListOfMoves2, ListOfMoves).