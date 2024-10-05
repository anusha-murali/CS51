(*  
                         CS 51 Problem Set 6
                                Search
 
                     Testing Tile and Maze Puzzles

In this file, we provide some tests of the puzzle solver by generating
random tile and maze puzzles and running the various solving methods
(depth-first, breadth-first, etc.) on the examples. This code requires
working versions of the `Collections` and `Puzzlesolve` modules, so it
won't compile until you've completed those parts of the problem
set. Once those are done, however, you can build `tests.byte` with

   % ocamlbuild -use-ocamlfind tests.byte

and run it to watch some puzzles being solved and get some
timings. This will be useful in designing your own experiments, as
required in Problem 3 of the problem set. *)

(* open CS51Utils ;;
open Tiles ;;
open Mazes ;;
open Puzzledescription ;;
open Puzzlesolve ;;
  *)
      
(*......................................................................
                       SAMPLE TILE PUZZLE TESTING
*)

(* initialize to known seed for reproducibility *)
let _  = Random.init 0 ;;  

(* A solved tile puzzle board for comparison*)
let cDIMS = 3, 3 ;;
let solved : board =
  [| [|Tile 1; Tile 2; Tile 3|];
     [|Tile 4; Tile 5; Tile 6|];
     [|Tile 7; Tile 8; EmptyTile|]; |] ;;
                
(* rand_elt -- Return a random state out of a list returned by the
   neighbors function of a tile puzzle description *)
let rand_elt l : board = 
    fst (List.nth l (Random.int (List.length l))) ;;

(* random_tileboard -- Generate a random TileBoard by performing some
   random moves on the solved board *)
let random_tileboard () : board =
  let cINITIAL_MOVE_COUNT = 45 in
  let module Puzzle : (PUZZLEDESCRIPTION with type state = board
                                     and type move = direction) = 
    MakeTilePuzzleDescription (struct
                                let initial = solved
                                let dims = cDIMS
                              end) in
  let rec make_moves n b = 
    if n <= 0 then b
    else make_moves (n - 1) (rand_elt (Puzzle.neighbors b)) in
  make_moves cINITIAL_MOVE_COUNT Puzzle.initial_state ;;

(* test_tile_puzzle -- generate a random board and solve it, reporting
   results with various solvers *)
let test_tile_puzzle () : unit =

  (* Generate a puzzle with a random initial position *)
  let module Puzzle : (PUZZLEDESCRIPTION with type state = board
                                          and type move = direction) = 
    MakeTilePuzzleDescription 
      (struct
          let initial = random_tileboard () 
          let dims = cDIMS
      end) in
  
  Printf.printf("TESTING RANDOMLY GENERATING TILEPUZZLE...\n");
  (* Guarantee that the initial state is not the goal state *)
  assert (not (Puzzle.is_goal Puzzle.initial_state));
  
  (* Create some solvers *)
  let module DFSG = DFSSolver(Puzzle) in 
  let module BFSG = BFSSolver(Puzzle) in
  let module FastBFSG = FastBFSSolver(Puzzle) in

  (* Run the solvers and report the results *)
  Printf.printf("Regular BFS time:\n");
  let (bfs_path, _bfs_expanded) =
    call_reporting_time BFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));

  Printf.printf("Faster BFS time:\n");
  let (fbfs_path, bfs_expanded) = 
    call_reporting_time FastBFSG.solve () in
  (* For breadth first search, you should also check the length *)
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));
  assert (Puzzle.is_goal (Puzzle.execute_moves fbfs_path));
  assert (List.length fbfs_path = List.length bfs_path);

  (* We skip the depth-first search for lack of time :) *)
  (*
  Printf.printf("Depth First Searching\n");
  let dfs_path, dfs_expanded = call_reporting_time DFSG.solve () in 
  flush stdout;
  DFSG.draw dfs_expanded dfs_path;
  *)
  Printf.printf("DONE TESTING RANDOMLY GENERATED TILE PUZZLE\n");;

  (* Display the path found by one of the solvers *)
  
let _ = test_tile_puzzle() ;;

