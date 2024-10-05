(*  
                         CS 51 Problem Set 6
                                Search
 
                     Testing Tile and Maze Puzzles

The initial set of provided puzzle tests have been enhanced by generating
random tile and maze puzzles and running the various solving methods
(depth-first, breadth-first, etc.) on the examples. 
*)

open CS51Utils ;;
open Tiles ;;
open Mazes ;;
open Puzzledescription ;;
open Puzzlesolve ;;
       
 
(*......................................................................
                  TILE PUZZLE TESTING: 2 x 2 TILE
*)

(* initialize to known seed for reproducibility *)
let _  = Random.init 0 ;;  

(* A 2 x 2  tile puzzle board *)
let cDIMS = 2, 2 ;; 
let solved : board =
  [| [|Tile 1; Tile 2|];
     [|Tile 3; EmptyTile|]; |] ;;
 
(* rand_elt -- Return a random state out of a list returned by the
   neighbors function of a tile puzzle description *)
let rand_elt l : board = 
    fst (List.nth l (Random.int (List.length l))) ;;

(* random_tileboard -- Generate a random TileBoard by performing some
   random moves on the solved board *)
let random_tileboard () : board =
  let cINITIAL_MOVE_COUNT = 45 in
  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                     and type move = Tiles.direction) = 
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
  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                          and type move = Tiles.direction) = 
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
  Printf.printf("Regular DFS time:\n");
  let (dfs_path, _dfs_expanded) =
    Absbook.call_reporting_time DFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves dfs_path));

  Printf.printf("Regular BFS time:\n");
  let (bfs_path, _bfs_expanded) = 
    Absbook.call_reporting_time BFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));

  Printf.printf("Faster BFS time:\n");
  let (fbfs_path, bfs_expanded) = 
    Absbook.call_reporting_time FastBFSG.solve () in
  (* For breadth first search, you should also check the length *)
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));
  assert (Puzzle.is_goal (Puzzle.execute_moves fbfs_path));
  assert (List.length fbfs_path = List.length bfs_path);

  (* We skip the depth-first search for lack of time :) *)
  
  Printf.printf("Depth First Searching\n");
  let dfs_path, dfs_expanded = Absbook.call_reporting_time DFSG.solve () in 
  flush stdout;
  DFSG.draw dfs_expanded dfs_path;
  
  Printf.printf("DONE TESTING RANDOMLY GENERATED TILE PUZZLE\n");

  (* Display the path found by one of the solvers *)
  BFSG.draw bfs_expanded bfs_path ;;
  
let _ = test_tile_puzzle() ;;

(*......................................................................
                  TILE PUZZLE TESTING: 3 x 3 TILE
*)

(* initialize to known seed for reproducibility *)
let _  = Random.init 0 ;;  

(* A 3 x 3 tile puzzle board *)
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
  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                     and type move = Tiles.direction) = 
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
  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                          and type move = Tiles.direction) = 
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
    Absbook.call_reporting_time BFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));

  Printf.printf("Faster BFS time:\n");
  let (fbfs_path, bfs_expanded) = 
    Absbook.call_reporting_time FastBFSG.solve () in
  (* For breadth first search, you should also check the length *)
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));
  assert (Puzzle.is_goal (Puzzle.execute_moves fbfs_path));
  assert (List.length fbfs_path = List.length bfs_path);

  (* We skip the depth-first search for lack of time :) *)
  
  Printf.printf("Depth First Searching\n");
  let dfs_path, dfs_expanded = Absbook.call_reporting_time DFSG.solve () in 
  flush stdout;
  DFSG.draw dfs_expanded dfs_path;
  
  Printf.printf("DONE TESTING RANDOMLY GENERATED TILE PUZZLE\n");

  (* Display the path found by one of the solvers *)
  BFSG.draw bfs_expanded bfs_path ;;
  
let _ = test_tile_puzzle() ;;

(*......................................................................
                           MAZE PUZZLE TESTING
*)

let init_maze = [|
    [| EmptySpace; EmptySpace; Wall; EmptySpace; EmptySpace|];
    [| Wall; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
    [| Wall; Wall; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; EmptySpace; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; Wall; EmptySpace; EmptySpace; EmptySpace|];
   |] ;;

(* square_maze copies -- Given the 5 * 5 initial maze above, and a
   `ct` number of times to copy it, generates a maze that is of size
   `(5 * ct) x (5 * ct)`, with the initial maze tiled on it. *)
let square_maze (copies : int) : maze =
  let orig = 5 (* dimensions of original square maze *) in
  let new_maze = Array.make_matrix
                   (orig * copies) (orig * copies)
                   EmptySpace in
  let col_bound = (orig * copies) in 
  let row_bound = (orig * copies) - orig in
  
  (* copy_maze -- tile the original maze into the new maze *)
  let rec copy_maze (crow: int) (ccol: int) : maze =     
    if (ccol = col_bound && crow = row_bound) then new_maze
    else if (ccol = col_bound) then 
      copy_maze (crow + orig) 0
    else
      begin
        List.init orig Fun.id (* for each row *)
        |> List.iter (fun offset ->
                      Array.blit init_maze.((crow + offset) mod orig) 0
                                 new_maze.(crow + offset) ccol orig);
        (* keep on recurring *)
        copy_maze (crow) (ccol + orig)
      end in
  
  copy_maze 0 0 ;;
  
(* Note that once the mazes get too big, the OCaml graphics module can't 
   properly render them *)
  
module TestMazeI : MAZEINFO = 
  struct
    let maze = square_maze 1
    let initial_pos =  (0, 0)
    let goal_pos = (4, 4)
    let dims = (5, 5)
  end
    
module TestMazeII : MAZEINFO = 
  struct
    let maze = square_maze 2
    let initial_pos =  (0, 0)
    let goal_pos = (9, 9)
    let dims = (10, 10)
  end
    
module TestMazeIII : MAZEINFO = 
  struct
    let maze = square_maze 3
    let initial_pos = (0, 0)
    let goal_pos = (14, 14)
    let dims = (15, 15)
  end

module TestMazeIV : MAZEINFO =
  struct
    let maze = square_maze 4
    let initial_pos =  (0,0)
    let goal_pos = (19,19)
    let dims = (20, 20)
  end

module TestMazeV : MAZEINFO =
  struct
    let maze = square_maze 5
    let initial_pos =  (0,0)
    let goal_pos = (24,24)
    let dims = (25, 25)
  end
    
(* TestMazePuzzle functor, returns a module that has one function (run_tests) *)
module TestMazePuzzle (M : MAZEINFO) = 
  struct
    let run_tests () = 
      (* Make a MazePuzzleDescription 
         using the MAZEINFO passed in to our functor *)
      let module MPuzzle = MakeMazePuzzleDescription(M) in 
      
      (* Generate three solvers -- two BFS solvers and a DFS solver *)
      let module DFSG = DFSSolver (MPuzzle) in 
      let module FastBFSG = FastBFSSolver (MPuzzle) in 
      let module BFSG = BFSSolver (MPuzzle) in 
      Printf.printf("TESTING MAZE PUZZLE...\n");
      
      (* Solve the BFS maze and make sure that the path reaches the goal *)
      Printf.printf("Regular BFS time:\n");
      let (bfs_path, _bfs_expanded) = 
        Absbook.call_reporting_time BFSG.solve () in
      assert (MPuzzle.is_goal (MPuzzle.execute_moves bfs_path));
      
      (* Solve the BFS maze with the efficient queue and make sure the
         path reaches the goal *)
      Printf.printf("Fast BFS time:\n");
      let (fbfs_path, bfs_expanded) = 
        Absbook.call_reporting_time FastBFSG.solve () in
      assert (MPuzzle.is_goal (MPuzzle.execute_moves fbfs_path));
      
      (* Assert the length of the fast BFS and regular BFS path are the
         same, as BFS always finds the shortest path *)
      assert ((List.length fbfs_path) = (List.length bfs_path));
      
      (* Solve the DFS maze and make sure the path reaches the goal *)
      Printf.printf("DFS time:\n");
      let (dfs_path, dfs_expanded) = 
        Absbook.call_reporting_time DFSG.solve () in
      assert (MPuzzle.is_goal (MPuzzle.execute_moves dfs_path));

      Printf.printf("DONE TESTING MAZE PUZZLE, DISPLAYING MAZE NOW\n");
      BFSG.draw bfs_expanded bfs_path;
      DFSG.draw dfs_expanded dfs_path    
  end ;;
  
(* Run the testing for each of our test mazes *)
module MI   = TestMazePuzzle (TestMazeI)
module MII  = TestMazePuzzle (TestMazeII)
module MIII = TestMazePuzzle (TestMazeIII)
module MIV  = TestMazePuzzle(TestMazeIV)
module MV   = TestMazePuzzle(TestMazeV)

let _ =
  MI.run_tests ();
  MII.run_tests ();
  MIII.run_tests ();
  MIV.run_tests();
  MV.run_tests();
