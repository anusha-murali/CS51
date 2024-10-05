(*......................................................................
                MAZE PUZZLE TESTING: GOAL STATE IS AT TOP LHS
*)

module TestMazeI : MAZEINFO =
  struct
    let maze = square_maze 1
    let initial_pos =  (0,0)
    let goal_pos = (1,4)
    let dims = (5, 5)
  end

module TestMazeII : MAZEINFO =
  struct
    let maze = square_maze 2
    let initial_pos =  (0,0)
    let goal_pos = (1,9)
    let dims = (10, 10)
  end

module TestMazeIII : MAZEINFO =
  struct
    let maze = square_maze 3
    let initial_pos =  (0,0)
    let goal_pos = (1,14)
    let dims = (15, 15)
  end

module TestMazeIV : MAZEINFO =
  struct
    let maze = square_maze 4
    let initial_pos =  (0,0)
    let goal_pos = (1,19)
    let dims = (20, 20)
  end

module TestMazeV : MAZEINFO =
  struct
    let maze = square_maze 5
    let initial_pos =  (0,0)
    let goal_pos = (1,24)
    let dims = (25, 25)
  end

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

