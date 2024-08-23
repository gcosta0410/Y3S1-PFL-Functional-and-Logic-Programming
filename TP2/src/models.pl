/*
* get_board(+Board, -BoardMatrix)
* 
* Convert a board representation to a matrix representation
*/
get_board(Player1Pieces-Player2Pieces-Width-Height, BoardMatrix) :- !, create_empty_matrix(Width, Height, EmptyMatrix),
                                                                      add_pieces(Player1Pieces, black, EmptyMatrix, Player1Matrix),
                                                                      add_pieces(Player2Pieces, white, Player1Matrix, X),
                                                                      BoardMatrix = X.


% function that receives a list of pairs that represent a position, a symbol and a matrix
% it gets the correct row and column of the position and adds the symbol to the matrix
%  returns a matrix with the elements added

/*
* add_pieces(+Positions, +Symbol, +Matrix, -NewMatrix)
*
* Add pieces to a matrix
*/
add_pieces([], _, Matrix, Matrix) :- !.
add_pieces([Position | Positions], Symbol, Matrix, NewMatrix) :- Position = (Row, Column),
                                                                       add_pieces(Positions, Symbol, Matrix, TempMatrix),
                                                                       replace_element_matrix(Symbol, Row, Column, TempMatrix, NewMatrix).


/*
* check_valid_piece_drop(+Position, +Board, +PlayerTurn)
*
* Check if a piece drop is valid, i.e, if the position is free, inside the board and not adjacent to a friendly piece
*
*/
check_valid_piece_drop((Row,Column),Player1Pieces-Player2Pieces-Width-Height, turn1) :-
    range(Row, -1, Height),
    range(Column, -1, Width),
    \+ member((Row, Column), Player1Pieces),
    \+ member((Row, Column), Player2Pieces),
    \+ relaxed_two_in_line((Row,Column),Player1Pieces).

check_valid_piece_drop((Row,Column),Player1Pieces-Player2Pieces-Width-Height, turn2) :-
    range(Row, -1, Height),
    range(Column, -1, Width),
    \+ member((Row, Column), Player1Pieces),
    \+ member((Row, Column), Player2Pieces),
    \+ relaxed_two_in_line((Row,Column),Player2Pieces).

/*
* check_valid_piece_selection(+Position, +Board, +PlayerTurn)
*
* Check if a piece selection is valid, i.e, if the position is inside the board and has free spaces around it to move to
*
*/
check_valid_piece_selection((Row,Column),Player1Pieces-Player2Pieces-Width-Height, turn1) :-
    range(Row, -1, Height),
    range(Column, -1, Width),
    RowDown1 is Row + 1,
    RowUp1 is Row - 1,
    ColumnRight1 is Column + 1,
    ColumnLeft1 is Column - 1,
    member((Row, Column), Player1Pieces),
    %verify that the piece is not completely surrounded by other pieces
    (
        (range(RowUp1, -1, Height), \+ member((RowUp1, Column), Player1Pieces), \+ member((RowUp1, Column), Player2Pieces));
        (range(RowDown1, -1, Height), \+ member((RowDown1, Column), Player1Pieces), \+ member((RowDown1, Column), Player2Pieces));
        (range(ColumnLeft1, -1, Width), \+ member((Row, ColumnLeft1), Player1Pieces), \+ member((Row, ColumnLeft1), Player2Pieces));
        (range(ColumnRight1, -1, Width), \+ member((Row, ColumnRight1), Player1Pieces), \+ member((Row, ColumnRight1), Player2Pieces))
    ).

check_valid_piece_selection((Row,Column),Player1Pieces-Player2Pieces-Width-Height, turn2) :-
    range(Row, -1, Height),
    range(Column, -1, Width),
    RowDown1 is Row + 1,
    RowUp1 is Row - 1,
    ColumnRight1 is Column + 1,
    ColumnLeft1 is Column - 1,
    member((Row, Column), Player2Pieces),
    %verify that the piece is not surrounded by other pieces
    (
        (range(RowUp1, -1, Height), \+ member((RowUp1, Column), Player1Pieces), \+ member((RowUp1, Column), Player2Pieces));
        (range(RowDown1, -1, Height), \+ member((RowDown1, Column), Player1Pieces), \+ member((RowDown1, Column), Player2Pieces));
        (range(ColumnLeft1, -1, Width), \+ member((Row, ColumnLeft1), Player1Pieces), \+ member((Row, ColumnLeft1), Player2Pieces));
        (range(ColumnRight1, -1, Width), \+ member((Row, ColumnRight1), Player1Pieces), \+ member((Row, ColumnRight1), Player2Pieces))
    ).

/*
* check_valid_piece_move(+OldPosition, +NewPosition, +Board, +PlayerTurn)
*
* Check if a piece move is valid, i.e, if the new position is inside the board and not occupied by another piece
*
*/
check_valid_piece_move((OldRow, OldColumn), (NewRow, NewColumn), (Player1Pieces-Player2Pieces-Width-Height), PlayerTurn) :-
    get_player_pieces(Player1Pieces, Player2Pieces, PlayerTurn, PlayerPieces),
    member((OldRow, OldColumn), PlayerPieces),
    range(OldRow, -1, Height),
    range(OldColumn, -1, Width),
    range(NewRow, -1, Height),
    range(NewColumn, -1, Width),
    orthogonal_move((OldRow, OldColumn), (NewRow, NewColumn)),
    \+ member((NewRow, NewColumn), Player1Pieces),
    \+ member((NewRow, NewColumn), Player2Pieces).

/*
* check_valid_enemy_piece_selection(+Position, +Board, +PlayerTurn)
*
* Check if an enemy piece selection is valid, i.e, if the piece belongs to the enemy
*
*/
check_valid_enemy_piece_selection(Position,Player1Pieces-Player2Pieces-Width-Height, turn1) :-
    member(Position, Player2Pieces).

check_valid_enemy_piece_selection(Position,Player1Pieces-Player2Pieces-Width-Height, turn2) :-
    member(Position, Player1Pieces).



/*
* three_in_line(+Position, +PlayerPieces)
*
* Check if a Piece is part of a 3 in line with pieces of the same player
* 4 or 5 in line is not allowed
*/
%Columns
three_in_line((Row, Column), PlayerPieces) :-
    ColumnLeft1 is Column - 1,
    ColumnLeft2 is Column - 2,
    ColumnRight1 is Column + 1,
    ColumnRight2 is Column + 2,
    member((Row, ColumnLeft1), PlayerPieces),
    member((Row, ColumnRight1), PlayerPieces),
    %4 or 5 in line not allowed
    \+ member((Row, ColumnLeft2), PlayerPieces),
    \+ member((Row, ColumnRight2), PlayerPieces),
    !.


three_in_line((Row, Column), PlayerPieces) :-
    ColumnLeft1 is Column - 1,
    ColumnLeft2 is Column - 2,
    ColumnLeft3 is Column - 3,
    ColumnRight1 is Column + 1,
    member((Row, ColumnLeft2), PlayerPieces),
    member((Row, ColumnLeft1), PlayerPieces),
    \+ member((Row, ColumnRight1), PlayerPieces),
    \+ member((Row, ColumnLeft3), PlayerPieces),
    !.


three_in_line((Row, Column), PlayerPieces) :-
    ColumnRight1 is Column + 1,
    ColumnRight2 is Column + 2,
    ColumnRight3 is Column + 3,
    ColumnLeft1 is Column - 1,
    member((Row, ColumnRight1), PlayerPieces),
    member((Row, ColumnRight2), PlayerPieces),
    \+ member((Row, ColumnLeft1), PlayerPieces),
    \+ member((Row, ColumnRight3), PlayerPieces),
    !.

%Rows
three_in_line((Row, Column), PlayerPieces) :-
    RowDown1 is Row + 1,
    RowUp1 is Row - 1,
    RowUp2 is Row - 2,
    RowDown2 is Row + 2,
    member((RowUp1, Column), PlayerPieces),
    member((RowDown1, Column), PlayerPieces),
    \+ member((RowUp2, Column), PlayerPieces),
    \+ member((RowDown2, Column), PlayerPieces),
    !.

three_in_line((Row, Column), PlayerPieces) :-
    RowDown2 is Row + 2,
    RowDown1 is Row + 1,
    RowDown3 is Row + 3,
    RowUp1 is Row - 1,
    member((RowDown2, Column), PlayerPieces),
    member((RowDown1, Column), PlayerPieces),
    \+ member((RowUp1, Column), PlayerPieces),
    \+ member((RowDown3, Column), PlayerPieces),
    !.

three_in_line((Row, Column), PlayerPieces) :-
    RowUp1 is Row - 1,
    RowUp2 is Row - 2,
    RowUp3 is Row - 3,
    RowDown1 is Row + 1,
    member((RowUp1, Column), PlayerPieces),
    member((RowUp2, Column), PlayerPieces),
    \+ member((RowDown1, Column), PlayerPieces),
    \+ member((RowUp3, Column), PlayerPieces),
    !.

/*
* relaxed_three_in_line(+Position, +PlayerPieces)
*
* Check if a Piece is part of a 3 in line with pieces of the same player
* 4 or 5 in line is allowed
*/
%Columns
relaxed_three_in_line((Row, Column), PlayerPieces) :-
    ColumnLeft1 is Column - 1,
    ColumnRight1 is Column + 1,
    member((Row, ColumnLeft1), PlayerPieces),
    member((Row, ColumnRight1), PlayerPieces),
    !.


relaxed_three_in_line((Row, Column), PlayerPieces) :-
    ColumnLeft1 is Column - 1,
    ColumnLeft2 is Column - 2,
    member((Row, ColumnLeft2), PlayerPieces),
    member((Row, ColumnLeft1), PlayerPieces),
    !.


relaxed_three_in_line((Row, Column), PlayerPieces) :-
    ColumnRight1 is Column + 1,
    ColumnRight2 is Column + 2,
    member((Row, ColumnRight1), PlayerPieces),
    member((Row, ColumnRight2), PlayerPieces),
    !.

%Rows
relaxed_three_in_line((Row, Column), PlayerPieces) :-
    RowDown1 is Row + 1,
    RowUp1 is Row - 1,
    member((RowUp1, Column), PlayerPieces),
    member((RowDown1, Column), PlayerPieces),
    !.

relaxed_three_in_line((Row, Column), PlayerPieces) :-
    RowDown2 is Row + 2,
    RowDown1 is Row + 1,
    member((RowDown2, Column), PlayerPieces),
    member((RowDown1, Column), PlayerPieces),
    !.

relaxed_three_in_line((Row, Column), PlayerPieces) :-
    RowUp1 is Row - 1,
    RowUp2 is Row - 2,
    member((RowUp1, Column), PlayerPieces),
    member((RowUp2, Column), PlayerPieces),
    !.

/*
* two_in_line(+Position, +PlayerPieces)
*
* Check if a Piece is part of a 2 in line with pieces of the same player
* 3 or 4 in line is not allowed
*/
%Columns
two_in_line((Row, Column), PlayerPieces) :-
    ColumnLeft1 is Column - 1,
    ColumnLeft2 is Column - 2,
    ColumnRight1 is Column + 1,
    member((Row, ColumnLeft1), PlayerPieces),
    %3 or 4 in line not allowed
    \+ member((Row, ColumnLeft2), PlayerPieces),
    \+ member((Row, ColumnRight1), PlayerPieces),
    !.


two_in_line((Row, Column), PlayerPieces) :-
    ColumnLeft1 is Column - 1,
    ColumnRight1 is Column + 1,
    ColumnRight2 is Column + 2,
    member((Row, ColumnRight1), PlayerPieces),
    \+ member((Row, ColumnLeft1), PlayerPieces),
    \+ member((Row, ColumnRight2), PlayerPieces),
    !.

%Rows
two_in_line((Row, Column), PlayerPieces) :-
    RowDown1 is Row + 1,
    RowUp1 is Row - 1,
    RowUp2 is Row - 2,
    member((RowUp1, Column), PlayerPieces),
    \+ member((RowUp2, Column), PlayerPieces),
    \+ member((RowDown1, Column), PlayerPieces),
    !.

two_in_line((Row, Column), PlayerPieces) :-
    RowDown2 is Row + 2,
    RowDown1 is Row + 1,
    RowUp1 is Row - 1,
    member((RowDown1, Column), PlayerPieces),
    \+ member((RowUp1, Column), PlayerPieces),
    \+ member((RowDown2, Column), PlayerPieces),
    !.


/*
* relaxed_two_in_line(+Position, +PlayerPieces)
*
* Check if a Piece is part of a 2 in line with pieces of the same player
* 3 or 4 in line is allowed
*/
relaxed_two_in_line((Row, Column), PlayerPieces) :-
    RowUp1 is Row - 1,
    member((RowUp1, Column), PlayerPieces),
    !.

relaxed_two_in_line((Row, Column), PlayerPieces) :-
    RowDown1 is Row + 1,
    member((RowDown1, Column), PlayerPieces),
    !.

%Columns
relaxed_two_in_line((Row, Column), PlayerPieces) :-
    ColumnLeft1 is Column - 1,
    member((Row, ColumnLeft1), PlayerPieces),
    !.

relaxed_two_in_line((Row, Column), PlayerPieces) :-
    ColumnRight1 is Column + 1,
    member((Row, ColumnRight1), PlayerPieces),
    !.




/*
 value(+GameState, +Player, -Value)
 value criteria:
 - phase 1: number of pairs of pieces that are in proximity to each other, that is, at a distance of 2
 - phase 2: 2 * number of 3 pieces in a line (horizontal or vertical) + number of pairs of pieces that are adjacent to each other
 - phase 3: number of pairs of enemy pieces that are adjacent to each other + number of enemy pieces
*/

%for phase 1
%objective: number of pairs of pieces that are adjacent to each other
value((Player1Pieces-Player2Pieces-Width-Height)/(1:_, _)/_, turn1, Value):-
    % inclusive_count_two_in_line(Player1Pieces, Value).

    pieces_in_proximity(Player1Pieces, Value).

value((Player1Pieces-Player2Pieces-Width-Height)/(1:_, _)/_, turn2, Value):-
    % inclusive_count_two_in_line(Player2Pieces, Value).

    pieces_in_proximity(Player2Pieces, Value).


%for phase 2
value((Player1Pieces-Player2Pieces-Width-Height)/(2, _, _)/_, turn1, Value):-
    inclusive_count_two_in_line(Player1Pieces, TwoInLine),
    count_three_in_line(Player1Pieces, ThreeInLine),
    Value is 2*ThreeInLine + TwoInLine. %ThreeInLine is given more weight

value((Player1Pieces-Player2Pieces-Width-Height)/(2, _, _)/_, turn2, Value):-
    inclusive_count_two_in_line(Player2Pieces, TwoInLine),
    count_three_in_line(Player2Pieces, ThreeInLine),
    Value is 2*ThreeInLine + TwoInLine. %ThreeInLine is given more weight

value((Player1Pieces-Player2Pieces-Width-Height)/((3:keep_turn), _, _)/_, turn1, Value):-
    inclusive_count_two_in_line(Player1Pieces, TwoInLine),
    count_three_in_line(Player1Pieces, ThreeInLine),
    Value is 2*ThreeInLine + TwoInLine. %ThreeInLine is given more weight

value((Player1Pieces-Player2Pieces-Width-Height)/((3:keep_turn), _, _)/_, turn2, Value):-
    inclusive_count_two_in_line(Player2Pieces, TwoInLine),
    count_three_in_line(Player2Pieces, ThreeInLine),
    Value is 2*ThreeInLine + TwoInLine. %ThreeInLine is given more weight


%for phase 3
value((Player1Pieces-Player2Pieces-Width-Height)/((3:advance_turn), _, _)/_, turn1, Value):-
    inclusive_count_two_in_line(Player2Pieces, TwoInLine),
    length(Player2Pieces, Player2PiecesLength),
    Value is 0 - (2 * TwoInLine + Player2PiecesLength ). %make the number negative because the Value of the GameStates is sorted by bigger value

value((Player1Pieces-Player2Pieces-Width-Height)/((3:advance_turn), _, _)/_, turn2, Value):-
    inclusive_count_two_in_line(Player1Pieces, TwoInLine),
    length(Player1Pieces, Player1PiecesLength),
    Value is 0 - (2 * TwoInLine + Player1PiecesLength).



/*
* valid_moves(+GameState, +Player, -ListOfMoves).
* 
* ListOfMoves is a list of all valid moves for the player in the current game state, depends on the phase of the game
*/
%for phase 1
valid_moves((Player1Pieces-Player2Pieces-Width-Height)/((1:_:_),_,_)/_, Player, Moves):-
    findall((Row, Column), check_valid_piece_drop((Row, Column), Player1Pieces-Player2Pieces-Width-Height, Player), Moves).    

%for phase 2
valid_moves((Player1Pieces-Player2Pieces-Width-Height)/(2,_,_)/PlayerTurn, turn1, Moves):-
    findall((OldPosition-NewPosition), check_valid_piece_move(OldPosition, NewPosition, Player1Pieces-Player2Pieces-Width-Height, turn1), Moves).

valid_moves((Player1Pieces-Player2Pieces-Width-Height)/(2,_,_)/PlayerTurn, turn2, Moves):-
    findall((OldPosition-NewPosition), check_valid_piece_move(OldPosition, NewPosition, Player1Pieces-Player2Pieces-Width-Height, turn2), Moves).

%for phase 3
valid_moves((Player1Pieces-Player2Pieces-Width-Height)/((3:_),_,_)/PlayerTurn, turn1, Moves):-
    findall(Position, (check_valid_enemy_piece_selection(Position, Player1Pieces-Player2Pieces-Width-Height, turn1)), Moves).

valid_moves((Player1Pieces-Player2Pieces-Width-Height)/((3:_),_,_)/PlayerTurn, turn2, Moves):-
    findall(Position, (check_valid_enemy_piece_selection(Position, Player1Pieces-Player2Pieces-Width-Height, turn2)), Moves).


