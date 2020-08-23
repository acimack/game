open! CS17SetupGame;
open SigGame; 
open Connect4;

/* Data Definitions:
whichPlayer denotes which player is making a turn, P1 or P2
status represents the status of the game. It can either be 
a Win(whichPlayer), a Draw, or Ongoing(whichPlayer)
A state of the game is represented by State(list(list(string)), status).
The list(list(string)) represents the current board and the status represents
the current status of the game at any given point.
A move is represented by Col(int). The integer inside of the Col represents the
number column that the player wants to drop a chip into. 
Example Data:
whichPlayer: P1, P2
status: Win(P1), Ongoing(P2), Draw
state: State([["R"],["R","B"],["R","B","B"], ["B","B","B"],[],[],["R"]], 
Ongoing(P1)), State([[“R” ],[ ],[ ],[ ],[ ],[ ],[ ]], Ongoing(P2))
move: Col(1), Col(3), Col(7)
*/

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame
  open PlayerGame;
 
let lookAhead = 5;

/* valueGrab: (move, float) => float
Input: a move, float tuple representing the move and its value 
when evaluated in estimateValue, tuple
Output: the float representation of the value of the tuple
*/ 
  let valueGrab: ((move, float)) => float = tuple =>
  switch(tuple){
  | (_, val1) => val1 
  };
  
/* sortVal: ((move, float), (move, float)) => int
Input: a tuple of (move, float) tuples, represented as (move1, move2) 
Output: an int, 1 if the value of move1 is greater than move2, -1 if 
move1 is less than move2, 0 if otherwise
*/ 
  let sortVal: (((move, float)), ((move, float))) => int = (move1, move2) =>
  switch(valueGrab(move1), valueGrab(move2)){
  | (a,b) => if (a > b){1}
           else if(a < b){-1}
           else{0}
  };
  
  /* minimaxHelper: (list((move, float)), whichPlayer) => (move, float)
Input: the list of (move, float) tuples representing the moves and their 
respective values, alot, and the current player, p
Output: the (move, float) tuple representing the tuple with the smallest 
value for P2 and the largest value for P1
*/ 
  let minimaxHelper: (list((move, float)), whichPlayer) => ((move, float)) = (alot, p) =>
  switch(p){
  | P1 => switch(alot){
    | [] => failwith("There are no possible moves")
    | [(move1, val1)] => (move1, val1)
    | [hd,...tl] => List.hd(List.rev(List.sort(sortVal,alot)))}
  | P2 => switch(alot){
    | [] => failwith("There are no possible moves")
    | [(move1, val1)] => (move1, val1)
    | [hd,...tl] => List.hd(List.sort(sortVal, alot))
  }
  };
  /* let nextMove: (PlayerGame.state => PlayerGame.move) = s => {
    failwith("not yet implemented")
  } */

/* nextMove: state => move
Input: the list of (move, float) tuples representing the moves and their respective values, alot, and the current player, p
Output: the (move, float) tuple representing the tuple with the smallest value for P2 and the largest value for P1
*/ 
  let nextMove: (state => move) = s => { 
    let rec minimax: (state, int) => (move, float) = (s, i) => {
      switch((gameStatus(s), i)){
      | (Win(P1), _) => (moveOfString("0"), 1000000.0)
      | (Win(P2), _) => (moveOfString("0"), -1000000.0)
      | (Draw, _) => (moveOfString("0"), 0.0)
      | (Ongoing(P1), 0) => (moveOfString("0"), estimateValue(s))
      | (Ongoing(P2), 0) => (moveOfString("0"), estimateValue(s))
       | (Ongoing(P1), a) => minimaxHelper(List.map((move) => (move, valueGrab(minimax(nextState(s, move), a - 1))), legalMoves(s)), P1)
       | (Ongoing(P2), a) => minimaxHelper(List.map((move) => (move, valueGrab(minimax(nextState(s, move), a - 1))), legalMoves(s)), P2)
    }};
     switch(minimax(s, lookAhead)){
     | (move1, _) => move1
     }};
}

module TestGame = Connect4;
module TestAIPlayer = AIPlayer(TestGame); 
open TestAIPlayer; 

checkExpect(sortVal((Col(2), 100.0),(Col(4), -645.0)), 1, "sortVal1");
checkExpect(sortVal((Col(1), 0.0), (Col(3), 0.0)), 0, "sortVal2");

checkExpect(minimaxHelper([(Col(2), 100.0),(Col(4), -645.0),(Col(3), 1000.0), (Col(1), -20.0)], P1), (Col(3), 1000.0), "P1 minimaxHelp");
checkExpect(minimaxHelper([(Col(2), 100.0),(Col(4), -645.0),(Col(3), 1000.0), (Col(1), -20.0)], P2), (Col(4), -645.0), "P2 minimaxHelp");
