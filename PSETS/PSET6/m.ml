(*  
                         CS 51 Problem Set 6
                                Search
 
                             Maze Puzzles
 *)

(* Maze type definitions: 
   Mazes are two dimensional grids where the elements represent open
   space or walls *)
       
type space =
  | EmptySpace
  | Wall

type maze = space array array

type position = int * int

type direction = Up | Down | Left | Right

(* Maze puzzles -- Information about a particular instance of a maze,
   providing the dimensions of the maze, its contents, and the initial
   position and goal position *)
                  
module type MAZEINFO =
sig 
    val dims : int * int
    val maze : maze
    val initial_pos : position
    val goal_pos : position
end
  

(* MakeMazepuzzleDescription -- functor that given a MAZEINFO module
   generates a PUZZLEDESCRIPTION module specifying a maze puzzle *)
module MakeMazePuzzleDescription (M : MAZEINFO)
       : (PUZZLEDESCRIPTION with type state = position
                             and type move = direction) = 
  struct
    
    (* Type `state` is where the player is currently located in the maze *)
    type state = position
                   
    (* The player can move various ways in the maze *)
    type move = direction

    (* Exception for invalid move *)
    exception InvalidMove

    (* The initial state is the initial position of the player *)
    let initial_state : state = M.initial_pos
    (* The goal state is as specified in the maze spec *)
    let goal_state : state = M.goal_pos

    let is_goal (s : state) : bool = 
        s = goal_state

    (* move_to_fun m -- 
       Converts a move m to a function to update a 2-D position *)
    let move_to_fun (m : move) : ((int * int) -> (int * int)) =
        match m with
        | Up -> fun (i, j) -> i - 1, j
        | Down -> fun (i, j) -> i + 1, j
        | Left -> fun (i, j) -> i, j - 1
        | Right -> fun (i, j) -> i, j + 1

    let validate_pos (i, j) : bool = 
       let w, h = M.dims in
       i >= 0 && i < h && j >= 0 && j < w

    let neighbors (playerPos : state) : (state * move) list =
      (* potentially all the moves *)
      [Up; Down; Left; Right] 
      (* update the position *)
      |> List.map (fun m -> ((move_to_fun m) playerPos), m)
      (* don't go off the board *)
      |> List.filter (fun (newPos, _) -> validate_pos newPos)
      |> List.filter (fun ((row, col), _) ->
                      match M.maze.(row).(col) with
                      | Wall -> false (* don't move onto a wall *)
                      | _ -> true) 

    let compare_states (s1 : state) (s2 : state) : int = 
      compare s1 s2

    let print_state (i, j : state) : unit =
      Printf.printf "(%d, %d) " i j ;;

    let execute_move (i, j : state) (m : move) : state =
      let new_board = 
        match m with 
        | Left -> i, j - 1
        | Right -> i, j + 1
        | Up -> i - 1, j
        | Down -> i + 1, j in
      if validate_pos new_board then new_board
      else raise InvalidMove ;;

    let execute_moves (path : move list) : state =
      List.fold_left execute_move initial_state path ;;
                       
  end

