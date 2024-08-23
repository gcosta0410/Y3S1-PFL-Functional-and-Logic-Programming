/**
* game_cycle(+GameState)
*
*  Function that's responsible for calling all the other major functions, integral to game execution.
*  Like name indicates, it's the game cycle that asks for a player move, moves the pieces, changes turns, prints the board and repeats until game is over
*/
game_cycle(GameState):-
    game_over(GameState, Winner), !,
    congratulate(Winner),
    sleep(3),
    write('Thank you for playing!'), nl.

game_cycle(GameState):- 
    choose_move(GameState, Move),    
    move(GameState, Move, TempGameState),
    next_player(TempGameState, NewGameState),
    display_game(NewGameState), !,
    game_cycle(NewGameState).


/**
 * initial_state(+Size, -GameState)
 * Size = (Width*Height)
 * sets up the board initial state based on its size
 */

initial_state(Size,(Player1Pieces-Player2Pieces-Width-Height)/(Phase,P1,P2)/PlayerTurn):-  
    Size = (BoardWidth*BoardHeight),
    Width is min(BoardWidth, 18),
    Height is min(BoardHeight, 15), 
    Player1Pieces = [],
    Player2Pieces = [],
    number_of_pieces(Width, Height, NumberOfPieces),
    Phase = (1:0:NumberOfPieces).
    %PlayerTurn = FirstTurn.

/**
 * game_over(+GameState, -Winner)
 * 
 * checks if the game is over or not
 */
game_over((Player1Pieces-_-_-_)/(2, _, _)/_, Winner):-  
    length(Player1Pieces, LPlayer1Pieces),
    LPlayer1Pieces = 0,
    Winner is 2, !.

game_over((_-Player2Pieces-_-_)/(2, _, _)/_, Winner):-  
    length(Player2Pieces, LPlayer2Pieces),
    LPlayer2Pieces = 0,
    Winner is 1, !.

game_over(_/_/_, _) :- fail.  

/**
 * start_menu/0
 * 
 * Initializes the start menu screen
 */  
start_menu:- repeat,
             print_start_menu,
             get_int(Gamemode), nl,
             select_gamemode(Gamemode).

/**
 * select_gamemode(+Gamemode)
 * 
 * Selects gamemode according to user input or, in case of bad input, sends out an error message.
 */
select_gamemode(1):- first_to_play_menu(FirstTurn), 
                     board_size_menu(Size),
                     initial_state(Size, (Board/(Phase,human1,human2)/FirstTurn)),   
                     display_game((Board/(Phase,human1,human2)/FirstTurn)), !,
                     game_cycle((Board/(Phase,human1,human2)/FirstTurn)).

select_gamemode(2):- ai2_difficulty_menu(Difficulty), 
                     first_to_play_menu(FirstTurn), 
                     board_size_menu(Size),
                     initial_state(Size, (Board/(Phase,human1,Difficulty)/FirstTurn)),
                     display_game((Board/(Phase,human1,Difficulty)/FirstTurn)), !,
                     game_cycle((Board/(Phase,human1,Difficulty)/FirstTurn)).


select_gamemode(3):- ai1_difficulty_menu(Difficulty1),
                     ai2_difficulty_menu(Difficulty2), 
                     first_to_play_menu(FirstTurn), 
                     board_size_menu(Size),
                     initial_state(Size, (Board/(Phase,Difficulty1,Difficulty2)/FirstTurn)),
                     display_game((Board/(Phase,Difficulty1,Difficulty2)/FirstTurn)), !,
                     game_cycle((Board/(Phase,Difficulty1,Difficulty2)/FirstTurn)).


select_gamemode(4):- how_to_play_menu.

select_gamemode(5):- !.

select_gamemode(_):- print_invalid_input, nl, fail.

/**
 * how_to_play_menu/0
 * 
 * Initializes the How to Play menu screen.
 */
how_to_play_menu:- print_how_to_play,
                   start_menu.

/**
 * first_to_play_menu(-FirstTurn)
 * 
 * Initializes the First to play menu screen and returns the first player to play.
 */
first_to_play_menu(FirstTurn) :- 
    repeat,
    print_first_to_play_menu,
    get_int(First), nl,
    select_first_to_play(First, FirstTurn).

/**
 * select_first_to_play(+First, -FirstTurn)
 * 
 * Selects first to play according to user input or, in case of bad input, sends out an error message.
 */
select_first_to_play(1, FirstTurn):- FirstTurn = turn1.

select_first_to_play(2, FirstTurn):- FirstTurn = turn2.

select_first_to_play(_, _):- print_invalid_input, nl, fail.

/**
 * board_size_menu(-Size)
 * 
 * Initializes the board size selection menu screen and returns board size.
 */
board_size_menu(Size) :- 
    get_width(Width),
    get_height(Height),
    Size = (Width*Height).

/*
* get_width(-Width)
*
* Gets board width from player and returns it
*/
get_width(Width) :- 
    repeat,
    print_width_selection,
    get_int(Width),
    validate_width(Width).

/**
* validate_width(+Width)
* 
* Checks if a given width is valid (6-18)
*/
validate_width(Width) :- between(6, 18, Width), !.

validate_width(_) :- print_invalid_input, nl, fail.


/*
* get_height(-Height)
*
* Gets board height from player and returns it
*/
get_height(Height) :- 
    repeat,
    print_height_selection,
    get_int(Height),
    validate_height(Height).

/**
* validate_height(+Height)
* 
* Checks if a given width is valid (5-15)
*/
validate_height(Height) :- between(5, 15, Height), !.

validate_height(_) :- print_invalid_input, nl, fail.

/**
 * ai1_difficulty_menu(-Difficulty)
 * 
 * Initializes the AI1 difficulty menu screen and returns AI1 difficulty.
 */
ai1_difficulty_menu(Difficulty) :- 
    repeat,
    print_ai1_difficulty_menu,
    get_int(Option), nl,
    select_ai1_difficulty(Option, Difficulty).

/**
 * select_ai1_difficulty(+Difficulty)
 * 
 * Selects AI1 difficulty according to user input or, in case of bad input, sends out an error message.
 */
select_ai1_difficulty(1, AI1Diff):- AI1Diff = ai1-1.

select_ai1_difficulty(2, AI1Diff):- AI1Diff = ai1-2.

select_ai1_difficulty(_):- print_invalid_input, nl, fail.

/**
 * ai2_difficulty_menu(-Difficulty)
 * 
 * Initializes the AI2 difficulty menu screen and returns AI2 difficulty. 
 */
ai2_difficulty_menu(Difficulty) :- 
    repeat,
    print_ai2_difficulty_menu,
    get_int(Option), nl,
    select_ai2_difficulty(Option, Difficulty).

/**
 * select_ai2_difficulty(+Option, -Difficulty)
 * 
 * Selects AI2 difficulty according to user input or, in case of bad input, sends out an error message.
 */
select_ai2_difficulty(1, AI2Diff):- AI2Diff = ai2-1.

select_ai2_difficulty(2, AI2Diff):- AI2Diff = ai2-2.

select_ai2_difficulty(_):- print_invalid_input, nl, fail.