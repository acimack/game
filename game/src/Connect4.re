open!  CS17SetupGame;   
open SigGame; 

module Connect4 = {

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


    let initialRows = 5;
    let initialCols = 7;

    /* player 1 is P1, player 2 is P2 */
    type whichPlayer =
      | P1
      | P2;

    /* either a player has won, it's a draw, or it's ongoing */
    type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);

     /* the state of the game: a list of string lists
     representing the columns of the board, status
    
    represents the layout of the board, a list of the columns, which are lists 
    of the chips in the column, and the status of the game*/
    type state = State(list(list(string)), status);

    /* describes a move that a player can make, the player can drop a 
    chip into any of the seven columns, the int represents the column
    */
    type move = 
      | Col(int);

    
/* stringOfPlayer: whichPlayer => string
Input: a whichPlayer, either P1 or P2, p
Output: a string representation of the inputted player, “Player 1” for P1 and 
“Player 2” for P2
*/    let stringOfPlayer: whichPlayer => string = p => switch(p){
    | P1 => "Player 1"
    | P2 => "Player 2"
    };

/* stringHelper: list(string) => list(string
Input: a list of strings, one of the columns of the game board, l
Output: a list of strings representing that same column with added “O” s 
representing the empty spaces on the board
Recursion Diagram: 
Original Input: [“R”, “B”]
Recursive Input: [“B”]
Recursive Output: [“O”,”O”,”O”,”O”,”B”]
We need to look at the length of the inputted column and make sure that we are
cons-ing on the proper number of O’s based on the value of the number of 
initial Rows. Therefore we can use a counter variable to make sure that
we don’t cons on too many or too few “O”s
Original Output: [“O”,”O”,”O”,”R”,”B”] */
    let rec stringHelper: list(string) => list(string) = l =>
    switch(l){
    | _ => 
       let k = initialRows - List.length(l);
      switch(k){
      | 0 => l
      | k when k > 0 => stringHelper(["O",...l])
      | _ => failwith("stringHelper domain error")
      };
    }; 
/* fillBoard: list(list(string)) => list(list(string))
Input: a list of lists of strings representing the original game board, b
Output: a list of lists of strings representing the game board with added “O”s
 to represent the spaces on the board that do not have an “R” or “B” chip 
 in them */
let fillBoard: list(list(string)) => list(list(string)) = b => 
    List.map(stringHelper, b);

/* transpose: list(list(‘a)) => list(list(‘a))
Input: any list of lists of alpha data type, mtrx, with n rows and k columns
Output: a list of lists of alpha data type that represents the transpose of 
mtrx (a matrix that results from reflecting the original matrix across its 
main diagonal)
Recursion Diagram:
Original Input: [[1,2]]
Recursive Input: [[1]]
Recursive Output: [[1]]
Create a new list of the second element from the original list and append
it to a list of lists containing a list with the first element from the 
original list. Since one row was the original input, the output will only
have 1 column.
Original Output: [[1],[2]]
*/    
    let rec transpose : list(list('a)) => list(list('a)) = mat => 
    switch (mat) {
    | [] => []
    | [[], ..._] => failwith("A matrix cannot be 0-dimensional.")
    | [[_hd], ..._] => [List.flatten(mat)]
    | [[_hd, ..._tl], ..._] => 
        [List.map(List.hd, mat), ...transpose(List.map(List.tl, mat))]
    };
   
/* stringOfState: state => string
Input: a state representing the current state of the game, s
String: a string representation of the current board, prints out the “O”s,
 “R”s, and “B”s that represent the empty spaces and chips that were inputted
  into the board
*/ 
let stringOfState: state => string = s =>
  {let rec stringOfStateHelper: list(list(string)) => string = b => {
    /*     stringOfStateHelper - does the switching for the tranposed board  */
            switch(b){
            | [] => ""
            | [hd, ... tl] => List.fold_right((x, y) => x ++ " " ++ y, hd, "") 
                                            ++ "\n" ++ stringOfStateHelper(tl)
            }};
    switch(s){
    | State(b, _) =>
     stringOfStateHelper(transpose(fillBoard(b)))
    }};

/* stringOfMove: move => string
Input: a move, m
Output: a string representing the column number that the player wants to drop 
a chip into (the word “Column” followed by the inputted number) */
    let stringOfMove: move => string = m => switch(m){
    | Col(i) => "Column " ++ string_of_int(i)
    };

    
    
/* diagHelper: (list(list(string)), int) => lit(list(string))
Input: a list of list of strings representing the board of the game, mat, and 
an integer representing the number of columns, cols
Output: a list of the diagonals of the board, starting from the left upper 
most corner and moving diagonally
Recursion Diagram:
Original Input: [[“R”,”B”],[“B”,”R”]], 2
Recursive Input: [[“B”,”R”]], 2
Recursive Output:[[“B”],[“R”]]
In order to get the diagonals, we will need to keep the last list generated 
and then add the second item to the first recursive output list and then make
 the first item of the original row separate. This will involve using the 
 take and drop functions to make sure the diagonals are in the right order. 
Original Output: [[“R”], [“B”,”B”],[“R”]]
*/
    let rec diagHelper: (list(list(string)), int) => list(list(string)) = 
                                                            (mat, cols) => {
      let rec take = (alod, n) => {
      switch(alod, n){
      | (_, 0) => []
      | ([], _) => failwith("Can't take items from an empty list")
      | ([hd,...tl], n) => [hd,...take(tl, n-1)]
      }};
      let rec drop = (alod, n) => {
      switch(alod, n) {
      | (_, 0) => alod
      | ([], _) => failwith("Can't drop items from an empty list")
      | ([_hd,...tl], n) => drop(tl, n-1)
      }}; 
    switch(mat) {
    | [] => failwith("Domain Error")
    | [row] => List.map(x => [x], row) 
    | [[],..._otherRows] => failwith("Domain Error")
    | [[hd,...tl],...otherRows] => 
    let rr = diagHelper(otherRows, cols);
    let front = take(rr, cols - 1);
    let back = drop(rr, cols -1);
    [[hd],...List.map2((x,y) => [x, ...y], tl, front)] @ back;
    }};

/* diags: list(list(string)) => list(list(string))
Input: a list of string lists representing the rows of the current game board
Output: a list of string lists representing all of the diagonals of the game
 board
*/    
    let diags: list(list(string)) => list(list(string)) = fun
    | [] => []
    | [hd, ...tl] => diagHelper([hd,...tl], List.length(hd));

    /* Game Logic */

/* initialStateHelper: int => list(list(string)) 
Input: an integer c representing the number of columns that we want in our 
initial game board
Output: a list of lists of strings containing c empty lists, representing 
the initially empty columns of the board
Recursion Diagram: 
Original Input: 5
Recursive Input: 4
Recursive Output: [[ ],[ ],[ ],[ ]]
We need to cons on the proper number of empty lists, so we are going to make 
the recursive call on c-1 to make sure that we have the right number of 
columns in our initial game board
Original Output: [[ ],[ ],[ ],[ ],[ ]] */
    let rec initialStateHelper: int => list(list (string)) = c =>
    switch(c){
    | 0 => []
    | c => [[],...initialStateHelper(c-1)]
    };

    /* The state of the game when it begins, we have a list of empty lists
    representing the columns of the board. When a player choses a column of
    int n, we will cons either a "R" or a "B" to the (n-1)th list using
    List.nth. 
    
    When printing the current game state, for every column, we will
    find the length, subtract the length from 5, and cons that many "O"s onto
    the list. For example, if our column has length 2, we will cons 3 "O"s 
    onto it when printing.
    
    the status, which is ongoing and player 1 starts
    */
    let initialState = State(initialStateHelper(initialCols), Ongoing(P1));


/* legalMoves: state => list(move)
Input: a state of the game, s
Output: a list with all of the possible moves that a player can make given the 
current game board
*/
  let legalMoves: state => list(move) = s => {
        let rec legalMovesHelp: (list(list(string)), int) => list(move) = 
                                                              (board, i) => 
        switch(board){
        | [] => []
        | [hd, ... tl] => if (List.length(hd) < initialRows) 
                          {[Col(i+1),...legalMovesHelp(tl, i+1)]}
                          else {legalMovesHelp(tl, i+1)}
        }
  switch(s){
    | State(_, Win(_)) => []
    | State(_, Draw) => []
    | State(b, Ongoing(_)) => legalMovesHelp(b, 0)
  }
};



/* gameStatus: state => status
Input: the current state of the game
Output: the current status of the game, contained within the current state
*/
    let gameStatus: state => status = s => switch(s){
    | State(_, a) => a
    }

/* nextStateHelper: (list(list(‘a)), int, int, whichPlayer)
Input: the current game board (a list of string lists, board), an integer
 representing the column that the player wants to drop a piece into, i, an
  integer that acts as a counter, c, and which player’s turn it is, p
Output: the new game board with the new chip added 
Recursion Diagram: 
Original Input:[["R"], ["B", "R"], ["B"], ["R"], ["R"], ["R"], [“B”]], 3, 0, P2
Recursive Input:[["R"], ["B","R"], ["B"], ["R"], ["R"], ["R"], [“B”]], 3, 1, P2
Recursive Output: ["R"], ["B", "R"], [“B”, “B”], ["R"], ["R"], ["R"], [“B”]]
We need to check if the counter is equal to the number of the column - 1 
because this will give us the index of the list that we want to add a chip
 to. By keeping track of this number, we can ensure that the chip is placed
in the correct column
Original Output: ["R"], ["B", "R"], [“B”, ”B”], ["R"], ["R"], ["R"], [“B”]]
*/
    let rec nextStateHelper: (list(list('a)), int, int, whichPlayer) => 
                                    list(list('a)) = (board, i, c, p) => 
    switch(c){
    | count when (count == initialCols) => []
    | count when (count < initialCols) => switch(p){
    | P1 =>  if ((i-1) == c) 
    {[["R", ... List.nth(board, i-1)], ...nextStateHelper(board, i, c+1, P1)]} 
            else {[List.nth(board, c), ...nextStateHelper(board, i, c+1, P1)]}
    | P2 => if ((i-1) == c) 
    {[["B", ... List.nth(board, i-1)], ...nextStateHelper(board, i, c+1, P2)]} 
            else {[List.nth(board, c), ...nextStateHelper(board, i, c+1, P2)]}
    }
    | _ => 
failwith("nextStateHelper takes in a list of list 'a, int, int, and a player")
    }

/* countSequence: (list(string), int) => int
Input: a list of strings, let, and an int representing our counter, c
Output: an int representing the size of the largest sequence of 
like “R”s or “B”s in the list

Recursion Diagram:
OI: ([“R”, “O”, “B”, “B”], 1)
	RI: ([“O”, “B”, “B”], 1)
	RO: 2
To determine the longest sequence, we determine if the head of the list is the 
same as the head of the tail and doesn’t equal “O”. If so, we add one to the 
counter and recur on the tail and that counter value. If the head is equal to 
“O”, then we recur on the tail and the original counter value. If neither of 
these are true, we find the max of the counter value vs the recursive value 
of the rest of the list and the counter when set equal to 1.
OO: 2 */
   let rec countSequence: (list(string), int) => int = (lst, c) => 
   switch(lst){
   | [] => 0
   | [_a] => c
   | [a, b] => if ((a == b) && (b != "O")) {c+1}
                   else {c}
   | [hd, ...tl] => 
                   if ((hd == List.hd(tl)) && (hd != "O")) 
                   {countSequence(tl, c+1)}
                   else if (hd == "O"){countSequence(tl, c)}
                   else{max(c, countSequence(tl,1))}
  };



  /* the number of elements in the completely full board*/ 
  let total = initialRows * initialCols

 /* drawHelper: list(list(‘a)) => int
Input: a list of lists of alpha data type, b (represents the board of the game
 in this case)
Output: the result of adding the lengths of each of the inner lists together
*/
 let drawHelper: list(list('a)) => int = b => List.fold_right((x, y) => x + y, 
                                                  List.map(List.length, b), 0)

/* nextState: (state, move) => state
Input: the current state of the game, s, and the move that a player wants 
to make, m
Output: the new state of the game when the move that the player inputted is 
added to the current game 
*/
let nextState: (state, move) => state = (s, m) => switch(s, m){
  | (State(b, Win(p)), _) => State(b, Win(p))
  | (State(b, Draw), _) => State(b, Draw)
  | (State(b, Ongoing(p)), Col(i)) => 
  let newBoard1 = nextStateHelper(b, i, 0, P1);
  let newBoard2 = nextStateHelper(b, i, 0, P2);
  let newBoardFilled1 = fillBoard(nextStateHelper(b, i, 0, P1));
  let newBoardFilled2 = fillBoard(nextStateHelper(b, i, 0, P2));
  let index = i-1;
  let pieceLocation1 = initialRows - 
      List.length(List.nth(nextStateHelper(b, i, 0, P1), index));
  let pieceLocation2 = initialRows - 
      List.length(List.nth(nextStateHelper(b, i, 0, P2), index));
   switch(p){
    | P1 => if ((countSequence(List.nth(newBoardFilled1, index), 1) == 4)
      || (countSequence(List.nth(transpose(newBoardFilled1), 
                                  pieceLocation1), 1) == 4) 
      || (countSequence(List.nth(diags(transpose(newBoardFilled1)), 
                                  pieceLocation1 + index), 1) == 4)
      || (List.mem(4, List.map((x) => countSequence(x, 1), 
                  diags(List.map(List.rev, transpose(newBoardFilled1)))))))
                {State(newBoard1, Win(P1))}
                else if (drawHelper(newBoard1) == total)
                {State(newBoard1, Draw)}
                else {State(newBoard1, Ongoing(P2))}
    | P2 => if
      ((countSequence(List.nth(newBoardFilled2, index), 1) == 4)
      ||(countSequence(List.nth(transpose(newBoardFilled2), 
                                  pieceLocation2), 1) == 4) 
      ||(countSequence(List.nth(diags(transpose(newBoardFilled2)), 
                                  pieceLocation2 + index), 1) == 4)
      ||(List.mem(4, List.map((x) => countSequence(x, 1), 
                  diags(List.map(List.rev, transpose(newBoardFilled2)))))))
                {State(newBoard2, Win(P2))}
                else if (drawHelper(newBoard2) == total)
                {State(newBoard2, Draw)}
                else {State(newBoard2, Ongoing(P1))}
            }
  };

   
/* moveOfString: string => move
Input: a string, str, which represents the move that the user inputs into 
the game
Output: an internal representation of the move, Col with the number of 
the column that the user wants to drop the piece into
*/
  let moveOfString: string => move = str => Col(int_of_string(str));

        
/* estimates the value of a given state (static evaluation) */

        
/* estimateValue: state => float
Input: a state of the game, s
Output: a float representing the numeric “value” of the game to a given player
 using static evaluation, values that are more positive are more beneficial 
 to player 1 and values that are more negative are more beneficial to player 2
*/
let estimateValue: state => float = s => {
  let rec estimateValueHelper: (list(string), float) => float = (lst, v) =>
  switch(lst){
  | [] => v
  | ["O","R","R","R","O",..._] => estimateValueHelper(List.tl(lst), v +. 900.0)
  | ["O","B","B","B","O",..._] => estimateValueHelper(List.tl(lst), v+. -900.0)
  | ["O","R","R","R",..._] => estimateValueHelper(List.tl(lst), v +. 700.0)
  | ["R","R","R","O",..._] => estimateValueHelper(List.tl(lst), v +. 700.0)
  | ["B","B","B","O",..._] => estimateValueHelper(List.tl(lst), v +. -700.0)
  | ["O","B","B","B",..._] => estimateValueHelper(List.tl(lst), v +. -700.0)
  | ["O","B","B","O",..._] => estimateValueHelper(List.tl(lst), v +. -800.0)
  | ["O","R","R","O",..._] => estimateValueHelper(List.tl(lst), v +. 800.0)
  | ["O","R","O","R",..._] => estimateValueHelper(List.tl(lst), v +. 400.0)
  | ["R","R","O","O",..._] => estimateValueHelper(List.tl(lst), v +. 400.0)
  | ["R","O","O","R",..._] => estimateValueHelper(List.tl(lst), v +. 400.0)
  | ["R","O","R","O",..._] => estimateValueHelper(List.tl(lst), v +. 400.0)
  | ["O","B","O","B",..._] => estimateValueHelper(List.tl(lst), v +. -400.0)
  | ["B","B","O","O",..._] => estimateValueHelper(List.tl(lst), v +. -400.0)
  | ["B","O","O","B",..._] => estimateValueHelper(List.tl(lst), v +. -400.0)
  | ["B","O","B","O",..._] => estimateValueHelper(List.tl(lst), v +. -400.0)
  | ["O","R","O","O",..._] => estimateValueHelper(List.tl(lst), v +. 300.0)
  | ["R","O","O","O",..._] => estimateValueHelper(List.tl(lst), v +. 300.0)
  | ["O","O","R","O",..._] => estimateValueHelper(List.tl(lst), v +. 300.0)
  | ["O","O","O","R",..._] => estimateValueHelper(List.tl(lst), v +. 300.0)
  | ["O","B","O","O",..._] => estimateValueHelper(List.tl(lst), v +. -300.0)
  | ["B","O","O","O",..._] => estimateValueHelper(List.tl(lst), v +. -300.0)
  | ["O","O","B","O",..._] => estimateValueHelper(List.tl(lst), v +. -300.0)
  | ["O","O","O","B",..._] => estimateValueHelper(List.tl(lst), v +. -300.0)
  | ["O","R","R",..._] => estimateValueHelper(List.tl(lst), v +. 50.0)
  | ["O","R","O",..._] => estimateValueHelper(List.tl(lst), v +. 50.0)
  | ["R","O","R",..._] => estimateValueHelper(List.tl(lst), v +. 50.0)
  | ["R","R","O",..._] => estimateValueHelper(List.tl(lst), v +. 50.0)
  | ["R","O","O",..._] => estimateValueHelper(List.tl(lst), v +. 50.0)
  | ["O","O","R",..._] => estimateValueHelper(List.tl(lst), v +. 50.0)
  | ["O","B","B",..._] => estimateValueHelper(List.tl(lst), v +. -50.0)
  | ["O","B","O",..._] => estimateValueHelper(List.tl(lst), v +. -50.0)
  | ["B","O","B",..._] => estimateValueHelper(List.tl(lst), v +. -50.0)
  | ["B","B","O",..._] => estimateValueHelper(List.tl(lst), v +. -50.0)
  | ["B","O","O",..._] => estimateValueHelper(List.tl(lst), v +. -50.0)
  | ["O","O","B",..._] => estimateValueHelper(List.tl(lst), v +. 50.0)
  | ["O","B",..._] => estimateValueHelper(List.tl(lst), v +. -20.0)
  | ["B","O",..._] => estimateValueHelper(List.tl(lst), v +. -20.0)
  | ["O","R",..._] => estimateValueHelper(List.tl(lst), v +. 20.0)
  | ["R","O",..._] => estimateValueHelper(List.tl(lst), v +. 20.0)
  | ["R",..._] => estimateValueHelper(List.tl(lst), v +. 10.0)
  | ["B",..._] => estimateValueHelper(List.tl(lst), v -. 10.0)
  | _ => v
  }
  switch(s){
  | State(_, Win(P1)) => 10000.0
  | State(_, Win(P2)) => -10000.0
  | State(_, Draw) => 0.0
  | State(b, Ongoing(_)) => 
  switch(b){
    | b => List.fold_right((x, y) => x +. y, 
          List.map(x => estimateValueHelper(x, 0.0), 
          b@transpose(fillBoard(b))@diags(transpose(fillBoard(b)))
          @diags(List.map(List.rev, transpose(fillBoard(b))))), 0.0)
    }
  }
}; 

};

module MyGame : Game = Connect4;
open Connect4;


/* test expects for Connect4 */
checkExpect(stringHelper(["R","B"]), ["O","O","O","R","B"], "filledcol1");
checkExpect(stringHelper([]), ["O","O","O","O","O"], "filledcol2");
checkExpect(stringHelper(["R","B","R","R","R"]), ["R","B","R","R","R"],
                                                         "filledcol3");

checkExpect(fillBoard([[],[],["R","R","B"],["R", "B", "B"], ["R","R"], [],[]]),
 [["O","O","O","O","O"],["O","O","O","O","O"],["O","O","R","R","B"],
 ["O","O","R","B","B"],["O","O","O","R","R"],["O","O","O","O","O"],
 ["O","O","O","O","O"]], "filledboard1");
checkExpect(fillBoard([[],[],[],[],[],[],[]]), 
[["O","O","O","O","O"],["O","O","O","O","O"],["O","O","O","O","O"],
["O","O","O","O","O"],["O","O","O","O","O"],["O","O","O","O","O"],
["O","O","O","O","O"]], "filledboard2");

checkExpect(stringOfMove(Col(1)), "Column 1", "stringMove1");
checkExpect(stringOfMove(Col(7)), "Column 7", "stringMove2");

checkExpect(initialStateHelper(4), [[],[],[],[]], "initialStateHelp1");
checkExpect(initialStateHelper(2), [[],[]], "initialStateHelp2");

checkExpect(transpose([["O","O","O","O","O"],["O","O","O","O","O"],
["O","O","R","R","B"],["O","O","R","B","B"],["O","O","O","R","R"],
["O","O","O","O","O"],["O","O","O","O","O"]]), [["O","O","O","O","O","O","O"]
,["O","O","O","O","O","O","O"],["O","O","R","R","O","O","O"], 
["O","O","R","B","R","O","O"],["O","O","B","B","R","O","O"]], "transpose1" );

checkExpect(diagHelper([["O","O","O","O","O","O","O"],
["O","O","O","O","O","O","O"],["O","O","R","R","O","O","O"], 
["O","O","R","B","R","O","O"],["O","O","B","B","R","O","O"]], 7), 
[["O"],["O","O"],["O","O","O"],["O","O","O","O"],["O","O","R","O","O"],
["O","O","R","R","O"],["O","O","O","B","B"],["O","O","R","B"],["O","O","R"],
["O","O"],["O"]], "diagHelper1");

checkExpect(diags(transpose(
  fillBoard([[],[],["R","R","B"],["R", "B", "B"], ["R","R"], [],[]]))), 
  [["O"],["O","O"],["O","O","O"],["O","O","O","O"],["O","O","R","O","O"],
  ["O","O","R","R","O"],["O","O","O","B","B"],["O","O","R","B"],["O","O","R"],
  ["O","O"],["O"]], "diags1");

checkExpect(nextStateHelper(
  [["R"], ["R"], [], ["R","B"], ["R"], ["R"], []], 4, 0, P1), 
  [["R"], ["R"], [], ["R","R","B"], ["R"], ["R"], []], "nextStateHelper1");
checkExpect(nextStateHelper([[], [], [], [], [], [], []], 1, 0, P2), 
[["B"], [], [], [], [], [], []], "nextStateHelper2");

checkExpect(countSequence(["R","R","R","B","B","O","O"],1), 3, "count1");
checkExpect(countSequence(["R","R","R","R","O","B","B"],1), 4, "count2");
checkExpect(countSequence(["B","B","B","R","R","R","R"],1), 4, "count3");

checkExpect(drawHelper([["R"],["R","B"],["R","B","B"], 
["B","B","B"],[],[],["R"]]), 10, "draw1");
checkExpect(drawHelper([["B","B","B","R","R","R","R"],
["B","B","B","R","R","R","R"],["B","B","B","R","R","R","R"],
["B","B","B","R","R","R","R"],["B","B","B","R","R","R","R"]]), 35, "draw2");

checkExpect(moveOfString("1"), Col(1), "moveString1");
checkExpect(moveOfString("5"), Col(5), "moveString1");


