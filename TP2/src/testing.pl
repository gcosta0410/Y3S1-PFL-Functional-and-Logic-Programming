test_board1(Board) :- Board = [
    [empty, empty, empty, empty, empty],
    [empty, white, empty, empty, empty],
    [empty, empty, black, empty, empty],
    [empty, empty, white, empty, empty],
    [empty, black, empty, empty, empty],
    [black, empty, empty, empty, white]
].

% test_gamestate<number>(X), display_game(X), game_cycle(X).

test_gamestate1(GameState) :- 
    Player1Pieces = [(3,3), (3,2), (4,2), (5,1)],
    Player2Pieces = [(2,2), (3,1), (5,5)],
    Width = 7,
    Height = 7,
    ModeInfo = ((1:8:10),human1, human2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

test_gamestate2(GameState) :- 
    Player1Pieces = [(3,3), (4,2), (4,1), (4,5)],
    Player2Pieces = [(2,2), (3,1), (4,4)],
    Width = 6,
    Height = 5,
    ModeInfo = (2,human1, human2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

test_gamestate3(GameState) :- 
    Player1Pieces = [(3,3), (4,2), (4,5)],
    Player2Pieces = [(2,2), (3,1), (4,4)],
    Width = 6,
    Height = 5,
    ModeInfo = ((3: keep_turn),human1, human2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

test_gamestate4(GameState) :- 
    Player1Pieces = [(3,3), (4,2), (4,5)],
    Player2Pieces = [(2,2), (3,1), (4,4)],
    Width = 6,
    Height = 5,
    ModeInfo = ((3: advance_turn),human1, human2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.


test_gamestate5(GameState) :- 
    Player1Pieces = [],
    Player2Pieces = [],
    Width = 6,
    Height = 5,
    ModeInfo = ((1:0:18),(ai1-1), (ai2-2)),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

test_gamestate6(GameState) :-
    GameState = ([(1,2),(4,3),(2,4),(1,5),(1,0),(0,4),(2,2),(1,3),(2,5),(0,0)]-[(4,5),(4,4),(3,4),(3,5),(4,2),(4,1),(3,1),(3,2),(2,3),(3,3)]-6-5)/(1:20:24,ai1-1,ai2-2)/turn1.

test_gamestate7(GameState) :-
    GameState = ([(3,0),(3,4),(1,4),(1,0),(0,4)]-[(4,5),(4,4),(4,2),(4,1)]-6-5)/(1:9:18,ai1-1,ai2-2)/turn2.

test_gamestate8(GameState) :-
    Player1Pieces = [],
    Player2Pieces = [],
    Width = 6,
    Height = 5,
    ModeInfo = ((1:0:18),(ai1-1), (ai2-1)),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

test_gamestate9(GameState) :-
    Player1Pieces = [],
    Player2Pieces = [],
    Width = 6,
    Height = 5,
    ModeInfo = ((1:0:18),(ai1-2), (ai2-2)),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

%no valid moves for player 2 in phase 2
test_gamestate10(GameState) :-
    Player1Pieces = [(2,1), (1,2), (2,3), (4,2)],
    Player2Pieces = [(2,2)],
    Width = 6,
    Height = 5,
    ModeInfo = (2, human1, human2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

%no valid moves for player 2 in phase 1
test_gamestate11(GameState) :-
    Player1Pieces = [(0,0), (0,1), (0,2), (1,2)],
    Player2Pieces = [(2,0), (2,1), (1,0)],
    Width = 3,
    Height = 3,
    ModeInfo = ((1:8:20), human1, human2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

% 1 piece to end phase 1
test_gamestate12(GameState) :-
    Player1Pieces = [(0,0), (0,1), (0,2), (1,2)],
    Player2Pieces = [(2,0), (2,1), (1,0)],
    Width = 3,
    Height = 3,
    ModeInfo = ((1:7:8), human1, human2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

% human1 vs ai2-level2
test_gamestate13(GameState) :-
    Player1Pieces = [(0,0), (0,1), (0,2), (1,2)],
    Player2Pieces = [(2,0), (2,1), (1,0)],
    Width = 4,
    Height = 4,
    ModeInfo = (2, human1, ai2-2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.

% 1 move from player 1 to win the game
test_gamestate14(GameState) :-
    Player1Pieces = [(0,0), (0,1), (1,2)],
    Player2Pieces = [(2,0)],
    Width = 6,
    Height = 5,
    ModeInfo = (2, human1, human2),
    PlayerTurn = turn1,
    GameState = (Player1Pieces-Player2Pieces-Width-Height)/ModeInfo/PlayerTurn.