(*  
                         CS 51 Problem Set 6
                                Search
 
                         Puzzle Descriptions

  Provides a `PUZZLEDESCRIPTION` module signature, which will be the
  signature for arguments to the `MakePuzzleSolver` functor that creates
  a full `Puzzle` module. (See the file `puzzlesolve.ml`.)

  Also provides generic functors for creating a variety of `Tile` and
  `Maze` `PUZZLEDESCRIPTION`s based on a simple input.
   
  Puzzles have a set of states and moves that can be applied to
  deterministically change puzzle state. A neighbors function specifies
  what state is moved to depending on the current state and move. There
  is a specially designated initial state and certain states can be goal
  states. A series of moves can be executed from the initial
  state. Functionality for graphically depicting puzzle states is also
  provided for. *)

module type PUZZLEDESCRIPTION =
  sig
    (* state -- The possible states of the puzzle *)
    type state
           
    (* move -- The possible moves allowed by the puzzle *)
    type move

    (* InvalidMove -- Exception raised when an invalid move is
       attempted *)
    exception InvalidMove
           
    (* neighbors state -- Given a state, returns a list of moves and
       the states that they would result in *)
    val neighbors : state -> (state * move) list
                                            
    (* initial_state -- The designated initial state for the puzzle *)
    val initial_state : state

    (* is_goal state -- Predicate is true if its argument is a goal
       state *)
    val is_goal : state -> bool
                             
    (* compare_states s1 s2 -- Compares two states, returning -1 if s1
       < s2; 0 if s1 = s2; +1 if s1 > s2 (consistent with
       Pervasives.compare). *)
    val compare_states : state -> state -> int
                                             
    (* execute_moves moves -- Returns the state that would result from
       executing moves starting in the initial state *)
    val execute_moves : move list -> state
                                     
    (* print_state -- Prints a representation of the state *)
    val print_state : state -> unit
                               
    (* draw states moves -- Graphically renders a set of states and a
       sequence of moves, where the moves, when executed starting in
       the initial state traverses the states provided; used for
       debugging *)
    val draw : state list -> move list -> unit
  end
