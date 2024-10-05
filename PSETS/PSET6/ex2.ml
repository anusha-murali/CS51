(*
In this file, I provide some tests of the puzzle solver by generating
random tile and maze puzzles and running the various solving methods
(depth-first, breadth-first, etc.) on the examples. *)

(* open CS51

open Collections *)
open Tiles
open Mazes
(* open Puzzledescription
open Puzzlesolve
*)


let init_maze = [|
    [| EmptySpace; EmptySpace; Wall; EmptySpace; EmptySpace|];
    [| Wall; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
    [| Wall; Wall; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; EmptySpace; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; Wall; EmptySpace; EmptySpace; EmptySpace|];
   |] ;;

    (* square_maze -- Given the 5 * 5 initial maze above, and a "ct"
       number of times to square it, generates a maze that is of size (5 *
       ct) x (5 * ct), with the initial maze tiled on it *)
let square_maze (ct : int) : maze =
  let new_maze = Array.make_matrix (5 * ct) (5 * ct) EmptySpace in
  let col_bound = (5 * ct) in
  let row_bound = (5 * ct) - 5 in
  (* helper function that tiles the original maze in to the new maze *)
  let rec copy_maze (crow: int) (ccol: int) : maze =
    if (ccol = col_bound && crow = row_bound) then new_maze
    else if (ccol = col_bound) then
      copy_maze (crow + 5) (0)
    else
      let _ =
        (Array.blit init_maze.(crow mod 5) 0 new_maze.(crow) ccol 5;
         Array.blit init_maze.((crow + 1) mod 5) 0 new_maze.(crow + 1) ccol 5;
         Array.blit init_maze.((crow + 2) mod 5) 0 new_maze.(crow + 2) ccol 5;
         Array.blit init_maze.((crow + 3) mod 5) 0 new_maze.(crow + 3) ccol 5;
         Array.blit init_maze.((crow + 4)mod 5) 0 new_maze.(crow + 4) ccol 5;) in
      (* Keep on recurring *)
      copy_maze (crow) (ccol + 5) in
  copy_maze 0 0 ;;

module TestMazeI : MAZEINFO =
  struct
    let maze = square_maze 1
    let initial_pos =  (0,0)
    let goal_pos = (4,4)
    let dims = (5, 5)
  end

module TestMazeII : MAZEINFO =
  struct
    let maze = square_maze 2
    let initial_pos =  (0,0)
    let goal_pos = (9,9)
    let dims = (10, 10)
  end

module TestMazeIII : MAZEINFO =
  struct
    let maze = square_maze 3
    let initial_pos =  (0,0)
    let goal_pos = (14,14)
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

(* TestMazePuzzle functor, returns a module that has one function (run_tests)
 *)
module TestMazePuzzle (M : MAZEINFO) =
  struct
    let run_tests () =

      (* Make a MazePuzzleDescription using the MAZEINFO passed in to our functor *)
      let module MPuzzle = MakeMazePuzzleDescription(M) in

      (* Generate two solvers -- a BFS solver and a DFS solver *)
      let module DFSG = DFSSolver(MPuzzle) in
      let module FastBFSG = FastBFSSolver(MPuzzle) in
      let module BFSG = BFSSolver(MPuzzle) in
      Printf.printf("TESTING MAZE PUZZLE...\n");

      (* Solve the BFS maze and make sure that the path reaches the goal *)
      Printf.printf("Regular BFS time:\n");
      let (bfs_path, bfs_expanded) = call_reporting_time BFSG.solve ()  in
      assert (MPuzzle.is_goal (MPuzzle.execute_moves bfs_path));

      (* Solve the BFS maze with the efficient queue and make sure the
         path reaches the goal *)
      Printf.printf("Fast BFS time:\n");
      let (fbfs_path, bfs_expanded) = call_reporting_time FastBFSG.solve ()  in
      assert (MPuzzle.is_goal (MPuzzle.execute_moves fbfs_path));

      (* Assert the length of the fast BFS and regular BFS path are the
         same, as BFS always finds the shortest path *)
      assert ((List.length fbfs_path) = (List.length bfs_path));

      (* Solve the DFS maze and make sure the path reaches the goal *)
      Printf.printf("DFS time:\n");
      let (dfs_path, dfs_expanded) = call_reporting_time DFSG.solve ()  in
      assert (MPuzzle.is_goal (MPuzzle.execute_moves dfs_path));

      Printf.printf("DONE TESTING MAZE PUZZLE, DISPLAYING MAZE NOW\n");;
      (* BFSG.draw bfs_expanded bfs_path;
      DFSG.draw dfs_expanded dfs_path *)

  end ;;

(* Run the testing for each of our test mazes *)
module MI   = TestMazePuzzle(TestMazeI)
module MII  = TestMazePuzzle(TestMazeII)
module MIII = TestMazePuzzle(TestMazeIII)
module MIV = TestMazePuzzle(TestMazeIV)
module MV = TestMazePuzzle(TestMazeV)

let _ =
  MI.run_tests();
  MII.run_tests();
  MIII.run_tests();
  MIV.run_tests();
  MV.run_tests();

