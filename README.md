# Six Making - Grupo 5

## Topic and Group

The game that we developed is called Six MaKING, a board game where the goal is to stack pieces so that you form a tower of 6 pieces.
- up202108802 - Carolina Teixeira Lopes Couto Viana - 50%
- up202108681 - Sérgio André Correia Peixoto - 50%

------------------------------------

## Installation and Execution

To install the game, you need to download and extract the content of the folder PFL_T1_T06_Six Making_5.zip.
Inside the src directory, consult the file main.pl and run the predicate play/0 to start the game.
```
?- consult(./main.pl).
?- play.
```

---------------------------

## Description of the game

Six MaKING is a game for 2 players, played on a 5x5 (or 4x4) board. For this project, however, we allow the user to choose the size of the board.

Each player alternate turns playing. When it is your turn you can:

 1. Place a new disk on the board (**Pawn**) on any empty square
 2. Move a tower (or part of one) to a non-empty square
       <br>a. **Move 1 piece of the tower** (**Pawn**): moves a single square in all 4 directions (up, down, right, left) on top of another tower.
       <br>b. **Move 2 pieces of the tower** (**Rook**): moves any number of squares in all 4 directions until the first tower in its path.
       <br>c. **Move 3 pieces of the tower** (**Knight**): moves in L shape on top of another tower.
       <br>d. **Move 4 pieces of the tower** (**Bishop**): moves any number of squares diagonally until the first tower in its path.
       <br>e. **Move 5 pieces of the tower** (**Queen**): moves any number of squares
       in all 8 directions until the first tower in its path.
 
Players can choose how many pieces of a tower they want to move, as long as it is a valid move.

If you only choose to move a part of a tower, the disks move as the original tower, before being split.

You cannot undo the opponents move.

The game ends when the first tower consisting of six disks (King) is built. The player who made the move that created the tower is the winner, despite of the piece on top. There are no draws in this game.

---

## Official Rules
  The official rule book for [Six MaKING](http://www.boardspace.net/sixmaking/english/Six-MaKING-rules-Eng-Ger-Fra-Ro-Hu.pdf).

---

## Game Logic

### Internal Game State Representation

The Game State is composed of two parts: the Board and the Player currently taking turn playing.

  * The **Board** is a **list of lists**, each list representing a row of the board. Each element of the list is a piece on the board or ´empty´ if the square is **empty**.

  * Each **Piece** is then represented as another list: this list being composed of the **height** of the tower (in the head of the list) follow by the pieces of the tower ifself. Colors are represented using numbers `1` to `6` to represent white pieces, and letters `a` to `f` to represent black pieces. Given this, an example tower would be represented as `[4, 1, b, c, 4]`.

  * The **Board** and the **Player** are joined together to form the **Game State** (Board-Player).

  In this game, there are no captured piece, and both players can move all pieces, so that information didn't need to be represented.

  #### Initial Board Example
  ```
  [[empty, empty, empty ,empty, empty],
    [empty, empty, empty ,empty, empty],
    [empty, empty, empty ,empty, empty],
    [empty, empty, empty ,empty, empty],
    [empty, empty, empty ,empty, empty]]
  ```
  
  ![Alt text](./images/image.png)

  #### Intermediate Board Example
  ```
  [[empty, [1,a], empty ,empty, empty],
    [empty, empty, [2,a,2] ,empty, empty],
    [[3,a,b,3], empty, [1,1] ,empty, [1,a]],
    [empty, [3,1,b,c], empty ,empty, empty],
    [empty, empty, empty ,[1,1], empty]]
  ```

  ![Alt text](./images/image2.png)

  #### Final Board Example
  ```
  [[empty, [1,a], empty ,empty, empty],
    [empty, empty, [2,a,2] ,empty, [6,1,2,c,d,e,6]],
    [[3,a,b,3], empty, [1,1] ,empty, [2,a,b]],
    [empty, [3,1,b,c], empty ,empty, empty],
    [empty, empty, empty ,[1,1], empty]]
  ```

  ![Alt text](./images/image3.png)


> NOTE: The code snippets and printed boards are not an exact match, but mere and unrelated examples.

### Game State Visualization

The game visualization and interaction as separated into different modules: `play.pl` and `board.pl`

* #### play.pl
  This module is responsible for handling aspects such as showing menus and validating menu inputs

  `display_menu` - Shows the main menu of the game.

  ![Alt text](images/image4.png)

  `first_menu_input(+Input)` - Validates the input of the main menu and calls the chosen menu.

  `play_menu` - Shows the play menu of the game where the user can select the game mode.

  ![Alt text](./images/image5.png)
  

  This pattern repeats for the other menus of the game where its needed to select the size of the board of the game or the difficulty of the AI.

  The predicates used for these menus are:
  - `second_menu_input(+Input)` - Validates the input of the play menu and allows the user to choose the difficulty of the game.
  - `board_size_menu(+TypeOfGame)` and `valid_board_size_input(+Input) `

  The last predicate called is `game_mode(+TypeOfGame, +SizeOfBoard)` that displays the initial state of the game and starts the game cycle.

* #### board.pl

  This module is responsible for drawing the board and the pieces of the game.

  `display_game(+GameState)` - Draws the board of the game.

  `initial_state(+Size, -GameState)` - Creates the initial state of the game given the size N of the board. This initial state consists of an empty board, as mentioned.

  `p_m(+Matrix, +N, +M)` - Lower level predicate used by display_game to print the board matrix with the coordinates of the board.

### Move Validation and Execution

```
move(GameState, ai-2, NewGameState):-
    write('\n\nThinking.........\n'),
    choose_move(GameState, ai, 2,[Type | Move]),
    move_type(Type, Move, GameState, NewGameState).

move(GameState, ai1-2, NewGameState):-
    write('\n\nThinking.........\n'),
    choose_move(GameState, ai, 2,[Type | Move]),
    move_type(Type, Move, GameState, NewGameState).

move(GameState, ai2-2, NewGameState):-
    write('\n\nThinking.........\n'),
    choose_move(GameState, ai, 2,[Type | Move]),
    move_type(Type, Move, GameState, NewGameState).

move(GameState, Player, NewGameState):-
    choose_play(GameState, Option, Player),
    single_play(Option, GameState, Player, NewGameState).
```

  The predicate that is used to validate and execute a complete move is called `move(+GameState, +Player, -NewGameState)`. Because of the complexity of our game, one move depends on various factors, therefore we chose to provide the Player as the argument, instead of the explicit move. Then, depending on the player, the move will be executed in different ways.

  If the player is the **regular user or AI Level 1**, the predicate will start by allowing the user to choose which move he wants to do: either place a new piece or move an existing piece.

  This is done in the `choose_play(+GameState, -Option, +Player) ` predicate.
  * If the player is the user, it asks and validates its input.
  * If the player is AI Level 1 and there are no pieces on the board, it chooses to place a new piece necessarily.
  * If the player is AI Level 1 and there are pieces on the board, it chooses randomly to either place a piece or move one.

Then, the move is executed in the ` single_play(+Option, +GameState, +Player, -NewGameState)` predicate. The predicate either:
  * Calls `new_piece_play(+Board, +Player, -NewBoard)` predicate, to place a new piece.
  * Calls `move_piece_play(+Board, +Player, -NewBoard)` predicate, to move an existing piece.

Again, if the player is a user, everything is done by asking for inputs and validating them. If it is AI Level 1, everything is chosen at random.

  If the player is the **AI Level 2**, the predicate will start by calling `choose_move(+GameState,+Player,+Level,-Move)` predicate, to choose a specific move according to our algorithm, as it will be explained next.
  Then, it calls the `move_type(+Type, +Move, +GameState, -NewGameState)` predicate, to actually execute the move, meaning either place a new piece or move an existing one.

Ultimately, to add a new piece the predicate that is called is `add_new_piece(+Board, +X, +Y, +Piece, -NewBoard)` and to move a piece ` move_piece(+Board, +X1, +Y1, +X2, +Y2, +N, -NewBoard)`. These are lower level predicates, meaning they interact directly with the GameState, altering it. Before calling them, lots of verifications and validations happen. 


This predicate is a part of one of our most important predicates: the `game_cycle(+GameState-+Player, +History-+Index)` predicate.
```
game_cycle(GameState-Player, _History-_Index):-
    game_over(GameState-Player, Winner), !,
    congratulate(Winner).

game_cycle(GameState-Player, History-Index):-
    repeat,
    move(GameState, Player, NewGameState),
    check_history(History-Index, NewGameState, NewHistory-NewIndex),
    write('\n------- CURRENT BOARD -------\n'),
    next_player(Player, NewPlayer),
    display_game(NewGameState), !,
    write_player_turn(NewPlayer),
    game_cycle(NewGameState-NewPlayer, NewHistory-NewIndex).
```

This is the predicate that makes the whole game work.

It starts by, as described above, letting the Player make a move (`move/3`). Then, it calls the ` check_history(+History-+Index, +GameState, -NewHistory-+NewIndex)` predicate. As suggested by our professor, and to implement the rule that imposes that a player cannot undo a previous move, we decided to implement a History of GameStates. So, after a certain move, we check that our current GameState is not equal to the GameState after the other player's turn.

After that validaion, we simply display the board, change turns and "start again".


### List of Valid Moves

  Due to the nature of game, any player can move any piece on the board as long as it is a valid move or place any piece on the board as long as the square is empty. Because of this, we didn´t need to pass the player as an argument to the predicate as there is no restriction on the moves that can be made (player based restrictions at least):

  * `valid_moves(+Board, -ListOfMoves)` - Which takes the current board and returns a list of all the possible moves (place or move piece) that can be made on the board.

  This predicate is used by the AI Level 2 to later choose a move to make.

  Each element in the return list represents a move that can be made and is in the format `[Type, Action]` where **Type** is either `place` or `move`, **Action** has the initial and final coordinates of the move in the case **Type** is `move` or the coordinates of the square to place the piece and the piece itself in the case **Type** is `place`.

> Note: For the other types of players on the game (Regular User and AI Level 1), there is no predicate that immediately returns all valid moves. This is because a valid move, as previously mentioned, depends on many complex factors and, for a better user experience, we opted by allowing the user to incrementaly choose what steps he wanted to take. He could choose: add a new piece or move one; where to add/move; how many disks to move; and more. The various options are validated along the way. If we had opted for giving it all possibilities of moves, the list would be endless and impractical. 

> An alternative we found was the `valid_coords(+Board, +X, +Y, +N, -Result)` predicate. In case the user chooses to move a piece, the predicate returns exactly all the possibilities to where it can be moved. 

### End of Game

  Checking whether the game has ended or not is done by the following predicate:

  * `game_over(+GameState, -Winner)` - Which check if there is a piece with a height of 6 or greater in the board, if there is, it returns the player that won the game. This will recursively check the board until it finds a piece with a height of 6 or greater and fails if it doesn´t find any.

  This check is done for every cycle in the game loop, so the game ends as soon as a player makes a move that results in a tower with a height of 6 or greater.

### Game State Evaluation

  In order for the AI Level 2 to better choose a move to make, we had to create a predicate that would evaluate the current state of the game and return a value that would represent how good is that play. This predicate is the following:

  * `value(+GameState, +Player, -Value)` - Which returns the value of the current board. To calculate the value we chose to use 3 different criteria in which we hope would give better choices. These are as such:

    * **Height of the tallest tower on the board** - The taller the tower, the better that play was, in hopes that the AI would choose to build a taller tower, as this is the main objetive of the game. This has a weight of 60% in the final value.

    * **Average height of the towers on the board** - The higher the average height of the towers in the board, the more options there are to reach higher towers in later moves. This has a weight of 30% in the final value.

    * **Number of towers in the board** - In the same way as the previous criteria, the more towers there are in the board, the more options there are to reach higher towers in later moves. This has a weight of 10% in the final value.
  
  Although the predicate includes an argument for Player, it is never used, as the player has no restrictions on which moves can be made. It is solely used for the purpose of deciding which move the AI Level 2 Player chooses, as mentioned. The predicate is used on the minimax algorithm.

### Computer Plays

  The computer has 2 distinct levels it can play on:
  
  * **Level 1** - The computer will choose random options every time it is asked for input. These options are continuously validated and, if needed, constrainted, to make sure the game flows without errors. If the Computer eventually chooses to move a piece, it is provided with a list of various coordinates to move and, again, chooses randomly from that list.

  * **Level 2** - The computer will choose the move that will result in the best value of the board. This is done using a greedy algorithm that will search for the best move two moves ahead of the current state of the game, taking into consideration that the next play will be from the opponent and that the opponent will choose the best play for him (minimax algorithm). Theses decisions are made using the `value/3` predicate described above.

    In this case, the best move is returned using the predicate `choose_move(+GameState,+Player,+Level,-Move) `.

---

## Conclusions

  The game Six MaKing was sucessfully implemented in SICStus Prolog 4.8. The game can be played in 5 different modes:

  * **Player vs Player**
  * **Player vs Computer (Level 1)**
  * **Player vs Computer (Level 2)**
  * **Computer (Level 1) vs Computer (Level 1)**
  * **Computer (Level 2) vs Computer (Level 2)**

  The size of the board can also be changed to whatever size wanted.

  One of the main challenges of this project was adapting our way of thinking outside of Object Oriented Programming and into Logic Programming. This was especially hard in the earlier stages when we had to think of ways to represent the game state and the pieces on the board

  One aspect that could be improved with more time would be the code itself. As this is our first time working with Prolog, we had to learn the language as we went along, so there are some parts of the code that could be improved and made more efficient as well as more clean, readable and reusable. Besides this, when we started developing the program, we had little idea of the best practices and the requirements for the predicates so a lot of refactoring had to be done and the end result was still not perfect

  Other aspect would be improving the AI Level 2. As this is dependent on the `value/3` predicate formula we chose, we would need more time to discover patterns in this AI and improve it to make better choices.

  Despite this, the game works without mistakes and we are very proud of the work we have done. We managed to learn a lot, overcome mistakes and problems and are really happy about the finished project.

---

## Bibliography

  * [Sicstus Prolog](https://sicstus.sics.se/documentation.html)
  * [SWI Prolog](https://www.swi-prolog.org/)