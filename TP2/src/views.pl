/*
* DRAWING THE BOARD
*/


/*
* draw_board(+BoardMatrix)
* 
* Draws the board given a matrix of the board where each element is either 'empty', 'black' or 'white'
*/
draw_board(BoardMatrix) :- nth0(0, BoardMatrix, FirstRow), length(FirstRow, RowLength), draw_board_header(RowLength), nl, draw_board_header_separator(RowLength), draw_board(BoardMatrix, 1).

/*
* draw_board_row(+Row, +RowNumber)
* Auxiliary predicate for draw_board/1
* Draws a row of the board given a list of the row and the row number
*/
draw_board([], _) :- !.
draw_board([BoardHead | BoardTail], I):- draw_board_row(BoardHead, I), nl, I1 is I + 1,
                                         length(BoardHead, RowLength),
                                         draw_board_row_separator(RowLength),
                                         draw_board(BoardTail, I1).


/*
* draw_board_header_separator(+Length)
*
* Draws the header separator of the board given the length of the row
*/
draw_board_header_separator(Length) :- write('-+-|'), print_n('-+-|', Length), nl.

/*
* draw_board_row_separator(+Length)
*
* Draws the row separator of the board given the length of the row
*/
draw_board_row_separator(Length) :- write('-+-|'), print_n('---|', Length), nl.


/*
* draw_board_header(+RowLength)
*
* Draws the header of the board given the length of the row
*/
draw_board_header(RowLength):- write('   |'), draw_board_header(RowLength, 0).

/*
* draw_board_header(+RowLength, +I)
* Auxiliary predicate for draw_board_header/1
* Draws the header of the board given the length of the row and the current index
*/
draw_board_header(Last, Last) :- !.
draw_board_header(RowLength, I) :- I < RowLength,
                                   char_code('A', ACode), I1 is ACode + I, char_code(NewChar, I1), write(' '), write(NewChar), write(' |'),
                                   NextI is I + 1,
                                   draw_board_header(RowLength, NextI).


/*
* draw_board_row_symbols(+Row)
*
* Draws the symbols of a row given a list of the row
*/
draw_board_row_symbols([]) :- !.
draw_board_row_symbols([empty | Tail]) :- write('   |'), draw_board_row_symbols(Tail).
draw_board_row_symbols([black | Tail]) :- write(' X |'), draw_board_row_symbols(Tail).
draw_board_row_symbols([white | Tail]) :- write(' O |'), draw_board_row_symbols(Tail).

/*
* draw_board_row(+Row, +RowNumber)
*
* Draws a row of the board given a list of the row and the row number
*/
draw_board_row([], _) :- !.
draw_board_row(Row, RowNumber) :- RowNumber >= 10, !, write(RowNumber), write(' |'), draw_board_row_symbols(Row). 
draw_board_row(Row, RowNumber) :- write(' '), write(RowNumber), write(' |'), draw_board_row_symbols(Row). 



/*
* print_turn(+Turn)
*
* Prints the current turn
*/
print_turn(turn1) :- write('---It\'s Player 1\'s turn---\n').
print_turn(turn2) :- write('---It\'s Player 2\'s turn---\n').


/**
 * display_game(+GameState)
 * 
 * Displays all the game information, the game board and player turn information
 */   

display_game(Board/((3:_),(ai1-_),_)/turn1):-
    nl, nl,
    get_board(Board, BoardMatrix),
    draw_board(BoardMatrix),
    !,
    nl, nl,
    write('Player 1 took a piece from Player 2.\n'),
    print_turn(turn1),
    print_remaining_pieces(Board),
    nl, nl.

display_game(Board/((3:_),_,(ai2-_))/turn2):-
    nl, nl,
    get_board(Board, BoardMatrix),
    draw_board(BoardMatrix),
    !,
    nl, nl,
    write('Player 2 will take a piece from Player 1.\n'),
    print_turn(turn2),
    print_remaining_pieces(Board),
    nl, nl.


display_game(Board/_ModeInfo/PlayerTurn):- 
    nl, nl,
    get_board(Board, BoardMatrix),
    draw_board(BoardMatrix),
    !,
    nl, nl,
    print_turn(PlayerTurn),
    print_remaining_pieces(Board),
    nl, nl.


/*
* MENUS
*/


/**
 * print_start_menu/0
 * 
 * Prints the start menu.
 */
print_start_menu:-   
    write('----------------------------------'),nl,
    write('|                                |'),nl,
    write('|      __          __   _ _      |'),nl,
    write('|      \\ \\        / /  | (_)     |'),nl,
    write('|       \\ \\  /\\  / /_ _| |_      |'),nl,
    write('|        \\ \\/  \\/ / _` | | |     |'),nl,
    write('|         \\  /\\  / (_| | | |     |'),nl,
    write('|          \\/  \\/ \\__,_|_|_|     |'),nl,
    write('|                                |'),nl,
    write('|           Joao Matos           |'),nl,
    write('|          Gustavo Costa         |'),nl,
    write('|                                |'),nl,
    write('|       1. Player vs Player      |'),nl,
    write('|       2. Player vs AI          |'),nl,
    write('|       3. AI vs AI              |'),nl,
    write('|       4. How to Play           |'),nl,
    write('|       5. Exit                  |'),nl,
    write('|                                |'),nl,
    write('----------------------------------'),nl, nl,
    write('Select one option from the above: ').


/**
 * print_how_to_play/0
 * 
 * Prints the How to Play menu.
 */
print_how_to_play:-
    write('-----------------------------------------------------------------------------'),nl,
    write('|                                                                           |'),nl,
    write('|                                HOW TO PLAY                                |'),nl,
    write('|                                                                           |'),nl,
    write('|   Wali is a 2 player turn based game played on a 6x5 (or bigger)          |'),nl,
    write('|   rectangular board.                                                      |'),nl,
    write('|   Each player starts with 12 or more stones, depending on board size.     |'),nl,
    write('|   The game is split into 2 phases:                                        |'),nl,
    write('|                                                                           |'),nl,
    write('|                              Drop/Put phase                               |'),nl,
    write('|   Taking turns, each player drops a stone into any empty space on the     |'),nl,
    write('|   board.                                                                  |'),nl,
    write('|   This empty space can\'t be orthogonally adjacent to any of the spaces    |'),nl,
    write('|   already occupied by one of this player\'s stones.                        |'),nl,
    write('|   If a player can\'t drop any more stones he must pass the turn.           |'),nl,
    write('|   The process repeats until all stones are placed on the board or no more |'),nl,
    write('|   can be dropped.                                                         |'),nl,
    write('|                                                                           |'),nl,
    write('|                                Move phase                                 |'),nl,
    write('|   Taking turns, each player moves a stone to an empty orthogonal adjacent |'),nl,
    write('|   space on the board.                                                     |'),nl,
    write('|   If they\'re able to make a 3 in a row (4 or more doesn\'t count), then    |'),nl,
    write('|   he may capture on stone from the opposing player.                       |'),nl,
    write('|                                                                           |'),nl,
    write('|   The goal of the game is to capture all of the opposing player\'s stones. |'),nl,
    write('|                                                                           |'),nl,
    write('-----------------------------------------------------------------------------'),nl, nl.


/**
 * print_first_to_play_menu/0
 * 
 * Prints the First to play menu.
 */
print_first_to_play_menu :-
    write('----------------------------'), nl,
    write('|                          |'), nl,
    write('|   Who will play first?   |'), nl,
    write('|                          |'), nl,
    write('|                          |'), nl,
    write('|       1. Player 1        |'), nl,
    write('|       2. Player 2        |'), nl,
    write('|                          |'), nl,
    write('----------------------------'), nl, nl,
    write('Select one option from the above: ').

/**
 * print_width_selection/0
 * 
 * Prints the board width selection menu.
 */
print_width_selection :-
    write('Choose board width (6-18): ').

/**
 * print_height_selection/0
 * 
 * Prints the board height selection menu.
 */
print_height_selection :-
    write('Choose board height (5-15): ').

/**
 * print_first_to_play_menu/0
 * 
 * Prints the AI1 difficulty selection menu.
 */
print_ai1_difficulty_menu :-
    write('----------------------------'), nl,
    write('|                          |'), nl,
    write('|   Choose AI1 difficulty  |'), nl,
    write('|                          |'), nl,
    write('|                          |'), nl,
    write('|         1. Easy          |'), nl,
    write('|     2. Intermediate      |'), nl,
    write('|                          |'), nl,
    write('----------------------------'), nl, nl,
    write('Select one option from the above: ').

/**
 * print_ai2_difficulty_menu/0
 * 
 * Prints the AI2 difficulty selection menu.
 */
print_ai2_difficulty_menu :-
    write('----------------------------'), nl,
    write('|                          |'), nl,
    write('|   Choose AI2 difficulty  |'), nl,
    write('|                          |'), nl,
    write('|                          |'), nl,
    write('|         1. Easy          |'), nl,
    write('|     2. Intermediate      |'), nl,
    write('|                          |'), nl,
    write('----------------------------'), nl, nl,
    write('Select one option from the above: ').
