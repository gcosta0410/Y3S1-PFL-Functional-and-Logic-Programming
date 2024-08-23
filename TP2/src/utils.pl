/*
 GENERAL PURPOSE PREDICATES
*/

/*
* print_n(+S, +N)
* 
* Prints the string S N times
*/
print_n(S, N):- N > 0, write(S), N1 is N - 1, print_n(S, N1).
print_n(_, _).


/*
* print_text(+Text, +Symbol, +Padding)
*
* Prints the text Text surrounded by the symbol Symbol and with a padding of Padding
*/
print_text(Text, Symbol, Padding):-  write(Symbol), print_n(' ', Padding),   write(Text)  , print_n(' ', Padding), write(Symbol).

/*
* print_banner(+Text, +Symbol, +Padding)
*
* Prints the text Text surrounded by the symbol Symbol and with a padding of Padding
* in a banner format
*/
print_banner(Text, Symbol, Padding):- atom_length(Text, Length),  N1 is Length + 2 * Padding + 2, 
                                    print_n(Symbol, N1),
                                    nl, N2 is N1 - 2,
                                    write(Symbol), print_n(' ',N2 ) , write(Symbol),
                                    nl,
                                    print_text(Text, Symbol, Padding),
                                    nl,
                                    write(Symbol), print_n(' ',N2 ) , write(Symbol),
                                    nl,
                                    print_n(Symbol, N1).


/*
* add_element(+X, +N, +L, -Result)
*
* Adds the element X to the position N in the list L
*/
add_element(NewElement, N, List, Result):- append(L1, L2, List), length(L1, N),  append(L1, [NewElement|L2], Result).

/*
* replace_element(+X, +N, +L, -Result)
*
* Replaces the element in the position N in the list L with the element X
*/
replace_element(NewElement, N, List, Result):- append(L1, L2, List), length(L1, N), L2 = [HeadL2 | TailL2],  append(L1, [NewElement|TailL2], Result).

/*
* replace_element_value(+X, +Y, +L, -Result)
*
* Replaces all the elements X in the list L with the element Y
*/
replace_element_value(X, Y, L, Result):- replace_element_value(X, Y, L, [], Result).
replace_element_value(_, _, [], Temp, Temp).
replace_element_value(X, Y, [X|Tail], Temp, Result):- append(Temp, [Y], Temp1), !, replace_element_value(X, Y, Tail, Temp1, Result).
replace_element_value(X, Y, [Head|Tail], Temp, Result):- append(Temp, [Head], Temp1), !, replace_element_value(X, Y, Tail, Temp1, Result).

/*
* delete_element(+X, +L, -Result)
*
* Deletes the element X from the list L
*/
delete_element(_, [], []).
delete_element(X, [X|Tail], Result):- delete_element(X, Tail, Result).
delete_element(X, [Head|Tail], [Head|Result]):- delete_element(X, Tail, Result).


/*
* add_element_matrix(+X, +Row, +Column, +Matrix, -Result)
*
* Adds the element X to the position Row, Column in the matrix Matrix
*/
add_element_matrix(NewElement, Row, Column, Matrix, Result):- nth0(Row, Matrix, RowList), add_element(NewElement, Column, RowList, NewRowList), add_element(NewRowList, Row, Matrix, Result).

/*
* replace_element_matrix(+X, +Row, +Column, +Matrix, -Result)
*
* Replaces the element in the position Row, Column in the matrix Matrix with the element X
*/
replace_element_matrix(NewElement, Row, Column, Matrix, Result):- nth0(Row, Matrix, RowList), replace_element(NewElement, Column, RowList, NewRowList), replace_element(NewRowList, Row, Matrix, Result).

/*
* create_empty_matrix(+C, +R, -Result)
*
* Creates a matrix with R rows and C columns filled with 'empty' atoms
*/
create_empty_matrix(C, R, Result):- create_empty_matrix(R, C, [], Result).

/*
* create_empty_list(+Rows, +Column, -Accumulator, -Result)
* Auxiliar predicate for create_empty_matrix/3
* 
*/ 
create_empty_matrix(0, _, Result, Result):- !.
create_empty_matrix(R, C, Temp, Result):- R > 0, !,  create_empty_list(C, [], Row), append(Temp, [Row], Temp1), R1 is R - 1, create_empty_matrix(R1, C, Temp1, Result).

/*
* create_empty_list(+N, +Accumulator, -Result)
* 
* Creates a list with N elements filled with 'empty' atoms
*/
create_empty_list(0, Result, Result).

/*
* create_empty_list(+N, +Accumulator, -Result)
* Auxiliar predicate for create_empty_list/3
* 
*/
create_empty_list(N, Temp, Result):- N > 0, append(Temp, [empty], Temp1), N1 is N - 1, create_empty_list(N1, Temp1, Result).

/*
* convert_letter_to_number(+Letter, -Number)
*
* Converts a letter to a number (A = 1, B = 2, ..., Z = 26), works for both upper and lower case letters
*/
convert_letter_to_number(Letter, Number):- char_code(Letter, LetterCode), ((LetterCode >= 65, LetterCode =< 90, char_code('A', ACode)) ; (LetterCode >= 97, LetterCode =< 122, char_code('a', ACode))) , Number is LetterCode - ACode + 1.

/*
* convert_number_to_letter(+Number, -Letter)
*
* Converts an ASCII code to a number
*/
parse_digit(Digit, Number):- char_code(Digit, DigitCode), DigitCode >= 48, DigitCode =< 57, Number is DigitCode - 48.

/**
 * between(+Low, +High, +Value)
 * 
 * Checks if the Value is between the Low and High values
 */
between(Low, High, Value):- 
    Low =< Value,
    High >= Value.

/*
* range(?Value, +Low, +High)
*
* Used a list with all the values between Low and High and to check if a Value is between Low and High
*/
range(X, L, H) :- X is L + 1, X < H.
range(X, L, H) :- L1 is L + 1, L1 < H, range(X, L1, H).

/*
* create_list_of_pairs(+List1, +List2, -List)
*
* Create a list of pairs of elements from List1 and List2. E.g create_list_of_pairs([1,2,3], [a,b,c], [1-a, 2-b, 3-c]).
*/
create_list_of_pairs([], [], []).
create_list_of_pairs([A|As], [B|Bs], [(A-B)|Pairs]):-
    create_list_of_pairs(As, Bs, Pairs).




/*
 GAME AUXILIARY FUNCTIONS
*/


/*
* orthogonal(+X1, +Y1, +X2, +Y2)
*
* Checks if the two given positions (X1, Y1) and (X2, Y2) are orthogonal and differ by only one position
*/
orthogonal_move((X1, Y1), (X2, Y2)):- (X1 =:= X2, Y1 =:= Y2 + 1) ; (X1 =:= X2, Y1 =:= Y2 - 1) ; (Y1 =:= Y2, X1 =:= X2 + 1) ; (Y1 =:= Y2, X1 =:= X2 - 1).

/*
* number_of_pieces(+Width, +Height, -NumberOfPieces)
* Calculates the number of pieces that will be in the board at the end of phase 1
*/ 
number_of_pieces(Width, Height, NumberOfPieces) :-
    Area is Width * Height,
    NumberOfPiecesAux is floor(Area * 0.8),
    NumberOfPieces is NumberOfPiecesAux - mod(NumberOfPiecesAux, 2).


/*
* congratulate(+Winner)
*
* Prints a congratulation message for the winner
*/
congratulate(Winner) :- 
    nl,nl,
    write('/---------------------------------------------------------------------\\'),nl,
    write('|                                                                     |'), nl,
    write('|                Congratulations Player '), write(Winner), write(', you won Wali!!!            |'), nl,
    write('|                                                                     |'), nl,
    write('\\---------------------------------------------------------------------/'),nl. 

/*
* print_remaining_pieces(+Board)
*
* Prints the number of pieces that each player has on the board
*/
 print_remaining_pieces(Player1Pieces-Player2Pieces-_-_) :-
    length(Player1Pieces, LengthPlayer1Pieces),
    length(Player2Pieces, LengthPlayer2Pieces),
    write('Player 1 has '), write(LengthPlayer1Pieces), write(' pieces on the board.\n'),
    write('Player 2 has '), write(LengthPlayer2Pieces), write(' pieces on the board.\n').

/*
* inclusive_count_two_in_line(+PlayerPieces, -NumberOfPairs)
*
* Counts the number of pairs of adjacent pieces in a list of positions, does not exclude 3 or 4 pieces in a line
*/
inclusive_count_two_in_line([], 0).
inclusive_count_two_in_line([H|T], NumberOfPairs):-
    inclusive_count_two_in_line(T, NumberOfPairs1),
    inclusive_count_two_in_line(H, T, NumberOfPairs2),
    NumberOfPairs is NumberOfPairs1 + NumberOfPairs2.

/*
* inclusive_count_two_in_line(+Position, +PlayerPieces, -NumberOfPairs)
*
* Auxiliar predicate for inclusive_count_two_in_line/2 where Position is being compared to the rest of PlayerPieces
*/
inclusive_count_two_in_line(_, [], 0).
inclusive_count_two_in_line((Row, Column), [(Row1, Column1)|T], NumberOfPairs):-
   ( (Row = Row1, Column is Column1 + 1);
    (Row = Row1, Column is Column1 - 1);
    (Column = Column1, Row is Row1 + 1);
    (Column = Column1, Row is Row1 - 1)),
    two_in_line((Row, Column), [(Row1, Column1)|T]),
    !,
    inclusive_count_two_in_line((Row, Column), T, NumberOfPairs1),
    NumberOfPairs is NumberOfPairs1 + 1.

inclusive_count_two_in_line((Row, Column), [_|T], NumberOfPairs):-
    inclusive_count_two_in_line((Row, Column), T, NumberOfPairs).


% a pair of pieces is in proximity if they are at a distance of 2
/*
* pieces_in_proximity(+PlayerPieces, -NumberOfPiecesInProximity)
*
* Counts the number of pieces that are in proximity of each other, i.e. at a distance of 2
*/
pieces_in_proximity([], 0).
pieces_in_proximity([H|T], NumberOfPiecesInProximity):-
    pieces_in_proximity(T, NumberOfPiecesInProximity1),
    pieces_in_proximity(H, T, NumberOfPiecesInProximity2),
    NumberOfPiecesInProximity is NumberOfPiecesInProximity1 + NumberOfPiecesInProximity2.

/*
* pieces_in_proximity(+Position, +PlayerPieces, -NumberOfPiecesInProximity)
*
* Auxiliar predicate for pieces_in_proximity/2 where Position is being compared to the rest of PlayerPieces
*/
pieces_in_proximity(_, [], 0).
pieces_in_proximity((Row, Column), [(Row1, Column1)|T], NumberOfPiecesInProximity):-
    ( (Row = Row1, Column is Column1 + 2);
    (Row = Row1, Column is Column1 - 2);
    (Column = Column1, Row is Row1 + 2);
    (Column = Column1, Row is Row1 - 2)),
    !,
    pieces_in_proximity((Row, Column), T, NumberOfPiecesInProximity1),
    NumberOfPiecesInProximity is NumberOfPiecesInProximity1 + 1.

pieces_in_proximity((Row, Column), [_|T], NumberOfPairs):-
    pieces_in_proximity((Row, Column), T, NumberOfPairs).

/*
* count_three_in_line(+PlayerPieces, -NumberOf3PiecesInLine)
*
* Counts the number of 3 pieces in a line in a list of positions, excludes 4 pieces in a line
*/
count_three_in_line([], 0).
count_three_in_line([H|T], NumberOf3PiecesInLine):-
    count_three_in_line(T, NumberOf3PiecesInLine1),
    three_in_line(H, T),
    !,
    NumberOf3PiecesInLine is NumberOf3PiecesInLine1 + 1.

count_three_in_line([_|T], NumberOf3PiecesInLine):-
    count_three_in_line(T, NumberOf3PiecesInLine).
    

/*
* count_two_in_line(+PlayerPieces, -NumberOf2PiecesInLine)
*
* Counts the number of 2 pieces in a line in a list of positions, excludes 3 or 4 pieces in a line
*/
count_two_in_line([], 0).

count_two_in_line([H|T], NumberOf2PiecesInLine):-
    count_two_in_line(T, NumberOf2PiecesInLine1),
    two_in_line(H, T),
    !,
    NumberOf2PiecesInLine is NumberOf2PiecesInLine1 + 1.

count_two_in_line([_|T], NumberOf2PiecesInLine):-
    count_two_in_line(T, NumberOf2PiecesInLine).
    
/*
* get_opposite_turn(+Turn, -OppositeTurn)
*
* Returns the opposite turn of the one given
*/
get_opposite_turn(turn1, turn2).
get_opposite_turn(turn2, turn1).

/*
* get_player_number(+PlayerTurn, -PlayerNumber)
*
* Returns the number of the player given the turn
*/
get_player_number(turn1, 1).
get_player_number(turn2, 2).

/*
* get_player_pieces(+Player1Pieces, +Player2Pieces, +PlayerTurn, -PlayerPieces)
*
* Returns the pieces of the player given the turn
*/
get_player_pieces(Player1Pieces, Player2Pieces, turn1, Player1Pieces).
get_player_pieces(Player1Pieces, Player2Pieces, turn2, Player2Pieces).

/*
* get_player_turn(+GameState, -PlayerTurn)
*
* Returns the turn of the player given the game state
*/
get_player_turn(_/_/PlayerTurn, PlayerTurn).

/*
* first_max_value_move(+ValueMoveList, -ValueMove)
*
* Returns the first move with the highest value in a list of Value-Move, the highest vaue can appear more than once
*/
first_max_value_move(List, Move):-
    max_member(MaxValue-AuxMove, List),

    first_occurrence(List, MaxValue, Move).

first_max_value_move([_-_|T], Move).

/*
* first_occurrence(+ValueMoveList, +MaxValue, -Move)
*
* Returns the first occurence of a Value-Move with the given MaxValue
*/
first_occurrence([HValue-HMove|T], MaxValue, Move):-
    HValue = MaxValue,
    Move = HMove, 
    !.

first_occurrence([_|T], MaxValue, Move):-
    first_occurrence(T, MaxValue, Move).

/*
* get_three_max_value_moves(+ValueMoveList, -ThreeMaxValueMoves)
*
* Returns a list of the three moves with the highest values in a list of Value-Move
*/
get_three_max_value_moves(List, [Move1, Move2, Move3]):-
    max_member(MaxValue1-AuxMove1, List),
    first_occurrence(List, MaxValue1, Move1),
    delete(List, MaxValue1-AuxMove1, List1),

    max_member(MaxValue2-AuxMove2, List1),
    first_occurrence(List1, MaxValue2, Move2),
    delete(List1, MaxValue2-AuxMove2, List2),

    max_member(MaxValue3-AuxMove3, List2),
    first_occurrence(List2, MaxValue3, Move3).

/*
* get_two_max_value_moves(+ValueMoveList, -TwoMaxValueMoves)
*
* Returns a list of the two moves with the highest values in a list of Value-Move
*/
get_two_max_value_moves(List, [Move1, Move2]):-
    max_member(MaxValue1-AuxMove1, List),
    first_occurrence(List, MaxValue1, Move1),
    delete(List, MaxValue1-AuxMove1, List1),

    max_member(MaxValue2-AuxMove2, List1),
    first_occurrence(List1, MaxValue2, Move2).


choose_from_best_moves(ValueMvList, Move):-
    length(ValueMvList, Length),
    !,
    (
    (Length > 2 ->
    (get_three_max_value_moves(ValueMvList, BestMoves), 
    random_select(Move, BestMoves, _Rest)));

    (get_two_max_value_moves(ValueMvList, BestMoves), 
    random_select(Move, BestMoves, _Rest))
    ),
    !.


/* MENU FUNCTIONS */

/**
 * cls/0
 * 
 * clears console screen
 */
cls :- write('\33\[2J').


/*
* print_invalid_input/0
*
* prints an invalid input error message to the user
*/
print_invalid_input :- write('Invalid input, try again.').