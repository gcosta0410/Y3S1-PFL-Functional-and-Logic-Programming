/*
 INPUT RELATED
*/

/*
* get_int(-Int)
*
* Reads an integer from the user
*/
get_int(Int):- 
    get_code(DozensDigit),
    check_first_digit(DozensDigit),
    get_code(UnitsDigit),                    
    handle_code_inputs(DozensDigit, UnitsDigit, Int).


/*
* check_first_digit(+DozensCode)
*
* Checks if the first digit 'DozensCode' is a valid digit
*/
check_first_digit(DozensCode):-    
    between(48, 57, DozensCode),
    !.

check_first_digit(10):- 
    !, 
    write('Not a valid number, try again.\n\n'), 
    fail.

check_first_digit(_):-  
    skip_line, 
    write('Not a valid number, try again.\n\n'), 
    fail.

/*
* handle_code_inputs(+DozensCode, +UnitsCode, -Res)
*
* Handles the input of the user, converting the codes to integers, parsing numbers of up to 2 to a number
*/
handle_code_inputs(DozensCode, 10, Res):-  
    between(48, 57, DozensCode),
    Res is DozensCode - 48,
    !.

handle_code_inputs(DozensCode, UnitsCode, Res):-    
    between(48, 57, DozensCode),
    between(48, 57, UnitsCode),
    get_code(NextCode),
    NextCode = 10,
    Res is ((DozensCode)-48)*10 + (UnitsCode-48),
    !.

hadle_code_inputs(_,_,_):-
    write('Not a valid number, try again.\n\n').



/*
* get_piece_position(+Message, -Position)
*
* Reads a piece position from the user
*/
get_piece_position(Message, (RowNumber, ColumnNumber)) :-
    repeat,
    write(Message), nl,
    write('Row: '), get_int(Row),
    write('Column: '), get_char(Column), nl,
    get_char(_),
    RowNumber is Row - 1,
    convert_letter_to_number(Column, ColumnAux),
    ColumnNumber is ColumnAux - 1.



/*
 MODEL CONTROLLERS
*/


/*
* next_player(+GameState, -GameState)
*
* Advances the turn to the next player
*/
next_player(Board/((3:keep_turn),P1, P2)/PlayerTurn, NewGameState):- !, NewGameState = Board/((3:advance_turn),P1, P2)/PlayerTurn.
next_player(Board/((3:advance_turn),P1, P2)/turn1, NewGameState):- !,  NewGameState = Board/(2, P1, P2)/turn2.
next_player(Board/((3:advance_turn),P1, P2)/turn2, NewGameState):- !,  NewGameState = Board/(2, P1, P2)/turn1.

next_player(Board/(1:TotalPieces:TotalPieces, P1, P2)/turn1, NewGameState):- !, write('End of Phase 1!\n\n'), print_banner('Start of phase 2!', '*', 8), NewGameState = Board/(2, P1, P2)/turn2.
next_player(Board/(1:TotalPieces:TotalPieces, P1, P2)/turn2, NewGameState):- !, NewGameState = Board/(2, P1, P2)/turn1.

next_player(Board/(1:PiecesPlaced:TotalPieces, P1, P2)/PlayerTurn, NewGameState):- 
    get_opposite_turn(PlayerTurn, OppositeTurn), % to advance the turn is to skip the current OppositeTurn
    valid_moves(Board/(1:PiecesPlaced:TotalPieces, P1, P2)/OppositeTurn, OppositeTurn, Moves),
    length(Moves, 0),
    !,
    get_player_number(OppositeTurn, PlayerNumber),
    write('No valid moves for player '), write(PlayerNumber), write(', advancing turn.\n'),
    PiecesPlaced1 is PiecesPlaced + 1,
    check_phase1_end(Board/(1:PiecesPlaced1:TotalPieces, P1, P2)/OppositeTurn, _AuxBoard/ModeInfo/_AuxPlayerTurn),
    %advance turn wether or not the phase has ended
    NewGameState = _AuxBoard/ModeInfo/PlayerTurn.

next_player(Board/(2, P1, P2)/PlayerTurn, NewGameState):-
    get_opposite_turn(PlayerTurn, OppositeTurn), 
    valid_moves(Board/(2, P1, P2)/OppositeTurn, OppositeTurn, Moves),
    length(Moves, 0),
    !,
    get_player_number(OppositeTurn, PlayerNumber),
    write('No valid moves for player '), write(PlayerNumber), write(', advancing turn.\n'),
    NewGameState = Board/(2, P1, P2)/PlayerTurn.

next_player(Board/ModeInfo/turn1, NewGameState):- !, NewGameState = Board/ModeInfo/turn2.
next_player(Board/ModeInfo/turn2, NewGameState):- !, NewGameState = Board/ModeInfo/turn1.

/*
* check_phase1_end(+GameState, -GameState)
*
* Checks if the phase 1 has ended, i.e, the expected number of pieces has been placed, and if so, advances to phase 2
*/
check_phase1_end(Board/(1:TotalPieces:TotalPieces, P1, P2)/PlayerTurn, NewGameState) :-
    NewGameState = Board/(2, P1, P2)/PlayerTurn, !.

check_phase1_end(Board/(1:PiecesPlaced:TotalPieces, P1, P2)/PlayerTurn, NewGameState) :-
    get_opposite_turn(PlayerTurn, OppositeTurn),
    (
        (valid_moves(Board/(1:PiecesPlaced:TotalPieces, P1, P2)/PlayerTurn, OppositeTurn, Moves),
        length(Moves, 0),
        write('No more valid moves for both players, advancing to phase 2.\n'),
        NewGameState = Board/(2, P1, P2)/PlayerTurn, !);

        NewGameState = Board/(1:PiecesPlaced:TotalPieces, P1, P2)/PlayerTurn
    ), !. %no change

/*
* check_piece_removal_phase(+GameState, +Position, -GameState)
*
* Checks if the piece removal phase has started by having a 3 in line, and if so, advances to it
*/
check_piece_removal_phase((Player1Pieces-Player2Pieces-Width-Height)/(2,P1,P2)/PlayerTurn, NewPosition ,NewGameState) :-
    get_player_pieces(Player1Pieces, Player2Pieces, PlayerTurn, PlayerPieces),
    three_in_line(NewPosition, PlayerPieces),
    NewGameState = (Player1Pieces-Player2Pieces-Width-Height)/((3:keep_turn),P1,P2)/PlayerTurn, !.

check_piece_removal_phase(GameState, _ , NewGameState) :- NewGameState = GameState.

/*
* move(+GameState, +Move, -NewGameState)
*
* Applies a move to the game state, updating the board
*/
%for phase 2
move((Player1Pieces-Player2Pieces-Width-Height)/(2,P1,P2)/turn1, Move, NewGameState) :- 
    Move = OldPosition-NewPosition,
    !,
    NewPosition = (NewRow, NewColumn),
    delete_element(OldPosition, Player1Pieces, TempPlayer1Pieces),
    append(TempPlayer1Pieces, [(NewRow, NewColumn)], NewPlayer1Pieces),
    TempGameState = (NewPlayer1Pieces-Player2Pieces-Width-Height)/(2,P1,P2)/turn1,
    check_piece_removal_phase(TempGameState, NewPosition, NewGameState),
    !.

move((Player1Pieces-Player2Pieces-Width-Height)/(2,P1,P2)/turn2, Move, NewGameState) :- 
    Move = OldPosition-NewPosition,
    !,
    NewPosition = (NewRow, NewColumn),
    delete_element(OldPosition, Player2Pieces, TempPlayer2Pieces),
    append(TempPlayer2Pieces, [(NewRow, NewColumn)], NewPlayer2Pieces),
    TempGameState = (Player1Pieces-NewPlayer2Pieces-Width-Height)/(2,P1,P2)/turn2,
    check_piece_removal_phase(TempGameState, NewPosition ,NewGameState),
    !.


%for phase 1
move((Player1Pieces-Player2Pieces-Width-Height)/((1:PiecesPlaced:TotalPieces),P1, P2)/turn1, Position, NewGameState) :- 
    append(Player1Pieces, [Position], NewPlayer1Pieces),
    NewPiecesPlaced is PiecesPlaced + 1,
    NewGameState = (NewPlayer1Pieces-Player2Pieces-Width-Height)/((1:NewPiecesPlaced:TotalPieces), P1, P2)/turn1,!.

move((Player1Pieces-Player2Pieces-Width-Height)/((1:PiecesPlaced:TotalPieces), P1, P2)/turn2, Position, NewGameState) :- 
    append(Player2Pieces, [Position], NewPlayer2Pieces),
    NewPiecesPlaced is PiecesPlaced + 1,
    NewGameState = (Player1Pieces-NewPlayer2Pieces-Width-Height)/((1:NewPiecesPlaced:TotalPieces), P1, P2)/turn2, !.

%for phase 3
move((Player1Pieces-Player2Pieces-Width-Height)/(3:EndTurn,P1,P2)/turn1, Position, NewGameState) :- 
    delete_element(Position, Player2Pieces, NewPlayer2Pieces),
    NewGameState = (Player1Pieces-NewPlayer2Pieces-Width-Height)/(3:EndTurn,P1,P2)/turn1, !.

move((Player1Pieces-Player2Pieces-Width-Height)/(3:EndTurn,P1,P2)/turn2, Position, NewGameState) :-
    delete_element(Position, Player1Pieces, NewPlayer1Pieces),
    NewGameState = (NewPlayer1Pieces-Player2Pieces-Width-Height)/(3:EndTurn,P1,P2)/turn2, !.





/*
* choose_move(+GameState, -Move)
*
* Chooses a move for the current player
*/

%for phase 1
%human
choose_move((Player1Pieces-Player2Pieces-Width-Height)/((1:PiecesPlaced:TotalPieces), human1, _)/turn1, Move):-
    repeat,
    get_piece_position('Where do you want to put your next piece? ', Position),
    check_valid_piece_drop(Position,Player1Pieces-Player2Pieces-Width-Height, turn1), !,
    Move = Position.

choose_move((Player1Pieces-Player2Pieces-Width-Height)/((1:PiecesPlaced:TotalPieces), _, human2)/turn2, Move):-
    repeat,
    get_piece_position('Where do you want to put your next piece? ', Position),
    check_valid_piece_drop(Position,Player1Pieces-Player2Pieces-Width-Height, turn2), !,
    Move = Position.
%ai
choose_move((Player1Pieces-Player2Pieces-Width-Height)/((1:PiecesPlaced:TotalPieces), (ai1-Level), _)/turn1, Move):-
    choose_move((Player1Pieces-Player2Pieces-Width-Height)/((1:PiecesPlaced:TotalPieces), (ai1-Level), _)/turn1, ai1, Level, Move).

choose_move((Player1Pieces-Player2Pieces-Width-Height)/((1:PiecesPlaced:TotalPieces), _, (ai2-Level))/turn2, Move):-
    choose_move((Player1Pieces-Player2Pieces-Width-Height)/((1:PiecesPlaced:TotalPieces), _, (ai2-Level))/turn2, ai2, Level, Move).


%for phase 2

%human
choose_move((Player1Pieces-Player2Pieces-Width-Height)/(2, human1, _)/turn1, Move):-
    repeat,
    get_piece_position('Which piece do you want to move?', OldPosition),
    check_valid_piece_selection(OldPosition,Player1Pieces-Player2Pieces-Width-Height, turn1), !,
    get_piece_position('Where do you want to move it?', NewPosition),
    check_valid_piece_move(OldPosition, NewPosition, Player1Pieces-Player2Pieces-Width-Height, turn1), !,
    Move = (OldPosition-NewPosition).

choose_move((Player1Pieces-Player2Pieces-Width-Height)/(2, _, human2)/turn2, Move):-
    repeat,
    get_piece_position('Which piece do you want to move?', OldPosition),
    check_valid_piece_selection(OldPosition,Player1Pieces-Player2Pieces-Width-Height, turn2), !,
    get_piece_position('Where do you want to move it?', NewPosition),
    check_valid_piece_move(OldPosition, NewPosition, Player1Pieces-Player2Pieces-Width-Height, turn2), !,
    Move = (OldPosition-NewPosition).

%ai
choose_move((Player1Pieces-Player2Pieces-Width-Height)/(2, (ai1-Level), _)/turn1, Move):-
    choose_move((Player1Pieces-Player2Pieces-Width-Height)/(2, (ai1-Level), _)/turn1, ai1, Level, Move).

choose_move((Player1Pieces-Player2Pieces-Width-Height)/(2, _, (ai2-Level))/turn2, Move):-
    choose_move((Player1Pieces-Player2Pieces-Width-Height)/(2, _, (ai2-Level))/turn2, ai2, Level, Move).


%for phase 3
%human
choose_move((Player1Pieces-Player2Pieces-Width-Height)/(3:advance_turn, human1, _)/turn1, Move):-
    repeat,
    get_piece_position('Which enemy piece do you want to remove?', Position),
    check_valid_enemy_piece_selection(Position,Player1Pieces-Player2Pieces-Width-Height, turn1), !,
    Move = Position.

choose_move((Player1Pieces-Player2Pieces-Width-Height)/(3:advance_turn, _, human2)/turn2, Move):-
    repeat,
    get_piece_position('Which enemy piece do you want to remove?', Position),
    check_valid_enemy_piece_selection(Position,Player1Pieces-Player2Pieces-Width-Height, turn2), !,
    Move = Position.

%ai
choose_move((Player1Pieces-Player2Pieces-Width-Height)/((3:_), (ai1-Level), _)/turn1, Move):-
    choose_move((Player1Pieces-Player2Pieces-Width-Height)/((3:_), (ai1-Level), _)/turn1, ai1, Level, Move).

choose_move((Player1Pieces-Player2Pieces-Width-Height)/((3:_), _, (ai2-Level))/turn2, Move):-
    choose_move((Player1Pieces-Player2Pieces-Width-Height)/((3:_), _, (ai2-Level))/turn2, ai2, Level, Move).





%AI-level1
/*
* choose_move(+GameState, +AIPlayer, +Level, -Move)
* 
* Choose a move for the AI player given its Level
*/
choose_move(GameState, ai1, 1, Move):-
    valid_moves(GameState, turn1, Moves),
    random_select(Move, Moves, _Rest).

choose_move(GameState, ai2, 1, Move):-
    valid_moves(GameState, turn2, Moves),
    random_select(Move, Moves, _Rest).

%AI-level2
choose_move(GameState, ai1, 2, Move):-
    valid_moves(GameState, turn1, Moves),
    setof(Value-Mv, NewState^( member(Mv, Moves),
        move(GameState, Mv, NewState),
        value(NewState, turn1, Value) ), ValueMvList),

    % random_permutation(ValueMvList, ShuffledList), %loop prevention
    % first_max_value_move(ShuffledList, Move).

    max_member(MaxValue-AuxMove, ValueMvList),
    % get the number of moves with the same value
    findall(Mv, MaxValue^(member(MaxValue-Mv, ValueMvList)), MaxValueMoves),
    % print(MaxValueMoves),
    length(MaxValueMoves, MaxValueMovesLength),
    (
       (MaxValueMovesLength =:= 1, (choose_from_best_moves(ValueMvList, Move)));
        (random_select(Move, MaxValueMoves, _Rest))
    ).


    % print(Move), nl, print(ValueMvList), nl. 

choose_move(GameState, ai2, 2, Move):-
    valid_moves(GameState, turn2, Moves),
    setof(Value-Mv, NewState^( member(Mv, Moves),
        move(GameState, Mv, NewState),
        value(NewState, turn2, Value) ), ValueMvList),

    random_permutation(ValueMvList, ShuffledList), %loop prevention
    first_max_value_move(ShuffledList, Move).
    
    max_member(MaxValue-AuxMove, ValueMvList),
    %get the number of moves with the same value
    findall(Mv, MaxValue^(member(MaxValue-Mv, ValueMvList)), MaxValueMoves),
        % print(MaxValueMoves),

    length(MaxValueMoves, MaxValueMovesLength),
    !,
    (
        (MaxValueMovesLength =:= 1, (choose_from_best_moves(ValueMvList, Move)));
        (random_select(Move, MaxValueMoves, _Rest))
    ).

    
    % print(Move), nl, print(ValueMvList), nl. 












%AI-level3-incomplete
choose_move(GameState, ai1, 3, Move):-
    valid_moves(GameState, turn1, Moves),
 
    setof(NewState-Mv, Mv^( member(Mv, Moves),
        move(GameState, Mv, NewState) ), NewStatesAndMoves),

    !,

    setof(FinalValue-FirstMove, NewState^
        (member(NewState-FirstMove, NewStatesAndMoves),
        print(NewState-FirstMove), nl,
        % choose_move(NewState, ai1, 2, SecondMove),
        % print(SecondMove), nl,
        SecondMove = (1,0),
        move(NewState, SecondMove, AuxState),
        print(AuxState), nl, nl,
        value(AuxState, turn1, FinalValue),
        write('evaluated: '), write(FinalValue), nl
        ), 
        MovesValueList), !,

    print(MovesValueList), nl,
    random_permutation(MovesValueList, ShuffledList), %loop prevention
    first_max_value_move(ShuffledList, Move), !.
        
        
   



