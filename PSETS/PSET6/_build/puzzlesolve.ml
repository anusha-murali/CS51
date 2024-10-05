(*
                         CS 51 Problem Set 6
                                Search

                            Puzzle Solving
 *)

(*======================================================================
Before working on this problem set, read the problem set 6 writeup in
the file `readme.pdf`. It provides context and crucial information for
completing the problems. In addition, make sure that you are familiar
with the problem set procedures in the document "Problem set
procedures for CS51".

You are allowed (and encouraged) to work with a partner on this
problem set. You are also allowed to work alone, if you prefer. See
https://cs51.io/procedures/pset-instructions/#working-with-a-partner
for further information on working with partners on problem sets.
======================================================================*)

(* This file contains the `PUZZLESOLVER` signature for modules that
solve particular puzzles, as well as a higher-order functor,
`MakePuzzleSolver`. A `PUZZLESOLVER` module solves the puzzle by
searching for a path from the initial state to the goal state.

The `MakePuzzleSolver` functor takes a `COLLECTION` functor and a
`PUZZLEDESCRIPTION` and returns a `PUZZLESOLVER`. The collection
specified by the functor is used to store the states that have been
reached so far. Thus, the ordering in which states are delivered by
the collection (using the `take` function) determines the order in
which the states are searched. A stack regime gives depth-first
search, a queue regime breadth-first search.

At the bottom of the file are definitions for depth-first search and
breadth-first search puzzle solvers. These are partially applied
versions of the `MakePuzzleSolver` functor that use certain
collections to engender different search methods.

This file makes use of the `Set` and `Collections` module, as well as the
`PuzzleDescription` module (which it opens). 
 *)

open Puzzledescription

(* PUZZLESOLVER -- a module signature that provides for solving puzzles
   and graphically drawing the results *)

module type PUZZLESOLVER =
  sig
    (* CantReachGoal -- Exception raised by solver when no solution can
       be found *)
    exception CantReachGoal

    (* state -- The possible puzzle states *)
    type state
    (* move -- The possible moves *)
    type move

    (* solve () -- Returns a solution to the puzzle as a pair containing
       a list of moves and a list of states. The moves, when executed
       starting in the initial state, result in a goal state. A list
       of all of the states visited in the solution process in any order 
       is provided as the returned state list. *)
    val solve : unit -> move list * state list
    (* draw states moves -- Graphically renders a solution given by
       the sequence of `moves` that was discovered through visiting
       the `states` *)
    val draw : state list -> move list -> unit
    (* print_state state -- Prints a representation of `state` on the
       standard output *)
    val print_state : state -> unit
  end

(* MakePuzzleSolver -- a higher-order functor that generates puzzle
   solvers, with type

     (functor (sig type t end -> COLLECTION)) 
           -> PUZZLEDESCRIPTION 
           -> PUZZLESOLVER

   A functor that given a functor from an element type to a
   `COLLECTION`, as well as a `PUZZLEDESCRIPTION`, returns a full
   `PUZZLESOLVER` module.

   The functor `MakeCollection` is used for generating the collection
   for storing pending states that have yet to be searched. Using
   different collection regimes -- stacks (`MakeStackList`), queues
   (`MakeQueueList`, `MakeQueueStack`), etc. -- leads to different
   search regimes -- depth-first, breadth-first, etc.
 *)
module MakePuzzleSolver
         (MakeCollection
            : functor (Element : sig type t end) ->
                      (Collections.COLLECTION with type elt = Element.t))
         (Puzzle : PUZZLEDESCRIPTION)
       : (PUZZLESOLVER with type state = Puzzle.state
                        and type move = Puzzle.move) =
  struct
    failwith "MakePuzzleSolver not implemented"
  end ;;

(* DFSSolver and BFSSolver -- Higher-order functors that take in a
   PUZZLEDESCRIPTION, and will return puzzles that are solved with DFS and
   BFS, respectively. The fast bfs solver uses a better implementation
   of queues for speed. *)
module DFSSolver = MakePuzzleSolver (Collections.MakeStackList) ;;
module BFSSolver = MakePuzzleSolver (Collections.MakeQueueList) ;;
module FastBFSSolver = MakePuzzleSolver (Collections.MakeQueueStack) ;;
