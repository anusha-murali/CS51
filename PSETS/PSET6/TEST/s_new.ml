(*
  This file contains the PUZZLESOLVER signature for modules that solve
  particular puzzles, as well as a higher-order functor,
  MakePuzzleSolver. A PUZZLESOLVER module solves the puzzle by searching for
  a path from the initial state to the goal state.

  The MakePuzzleSolver functor takes a COLLECTION functor and a
  PUZZLEDESCRIPTION and returns a PUZZLESOLVER. The collection specified
  by the functor is used to store the states that have been reached so
  far. Thus, the ordering in which states are delivered by the
  collection (with the take function) determines the order in which
  the states are searched. A stack regime gives depth-first search, a
  queue regime breadth-first search.

  At the bottom of the file are definitions for depth-first search and
  breadth-first search puzzle solvers, partially applied versions of the
  MakePuzzleSolver functor that use certain collections to engender
  different search methods.
 *)

open Set
open Collections
(* open Puzzledescription *)

(* PUZZLESOLVER -- a module signature that provides for solving puzzles
   and graphically drawing the results *)

module type PUZZLESOLVER =
  sig
    (* CantReachGoal -- Exception raised by solve when no solution can
       be found *)
    exception CantReachGoal

    (* state -- The possible puzzle states *)
    type state
    (* move -- The possible moves *)
    type move

    (* solve () -- Returns a solution to the puzzle as a pair containing
       a list of moves and a list of states. The moves, when executed
       starting in the initial state, result in a goal state. A list
       of all of the states visited in the solution process in order
       of visiting (useful in visualizing the search process) is
       provided as the returned state list. *)
    val solve : unit -> move list * state list
    (* draw states moves -- Graphically renders a solution given by the moves *)
    (* print_state state -- Prints a representation of a state on the
       standard output *)
    val print_state: state -> unit
  end

(* MakePuzzleSolver -- a higher-order functor that generates puzzle solvers, with type

     (functor(sig type t end -> COLLECTION)) -> PUZZLEDESCRIPTION -> PUZZLESOLVER

   A functor that given a functor from an element type to a
   COLLECTION, as well as a PUZZLEDESCRIPTION, returns a full PUZZLESOLVER
   module.

   The functor MakeCollection is used for generating the collection
   for storing pending states that have yet to be searched. Using
   different collection regimes -- stacks (MakeStackList), queues
   (MakeQueueList, MakeQueueStack), etc. -- leads to different search
   regimes -- depth-first, breadth-first, etc.
 *)

  module MakePuzzleSolver
         (MakeCollection
            : functor (Element : sig type t end) ->
                      (COLLECTION with type elt = Element.t))
         (G : PUZZLEDESCRIPTION)
         (Element : sig type t end)
       : (PUZZLESOLVER with type state = G.state
                        and type move = G.move) =
  struct
    exception CantReachGoal

(*    module Collection = MakeCollection(Element) *)

    type state = G.state

    type move = G.move

    module Pending = MakeCollection(
      struct
        type t = state * (move list)
      end)

    module Visited = Set.Make(
      struct
        type t = state
        let compare = G.compare_states
      end)

    let solve () : (move list * state list) =
      let pending = Pending.add (G.initial_state, []) Pending.empty in
      let visited = Visited.singleton G.initial_state in
      let expanded = [] in

      let rec search (p : Pending.collection) (v : Visited.t) (e : state list) =
        if Pending.is_empty p then raise CantReachGoal
        else
          let (current_state, movelist), col = Pending.take p in
            if G.is_goal current_state then List.rev movelist, List.rev e
            else
              let rec add_n (n : (state * move) list)
                            (p2 : Pending.collection)
                            (v2 : Visited.t)
                            (moves : move list) =
              match n with
              | [] -> p2, v2
              | (state, mv) :: tl ->
                  if not (Visited.mem state v2)
                    then add_n tl (Pending.add (state, mv :: moves) p2)
                                  (Visited.add state v2)
                                  moves
                  else add_n tl p2 v2 moves in
                  let p3, v3 = add_n (G.neighbors current_state) col v movelist in
              search p3 v3 (current_state :: e) in
        search pending visited expanded

    let print_state = G.print_state

  end ;;

(* DFSSolver and BFSSolver: Higher-order functors that take in a
   PUZZLEDESCRIPTION, and will return puzzles that are solved with DFS and
   BFS, respectively. The fast bfs solver uses a better implementation
   of queues for speed. *)
module DFSSolver = MakePuzzleSolver(MakeStackList) ;;
module BFSSolver = MakePuzzleSolver(MakeQueueList) ;;
module FastBFSSolver = MakePuzzleSolver(MakeQueueStack) ;;
