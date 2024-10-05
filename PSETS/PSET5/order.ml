(*
                         CS 51 Problem Set 5
                Modules, Functors, and Data Structures
 *)

(* order -- a type for comparison operation results *)
type order = Less | Equal | Greater ;;

(* string_compare x y -- compares two strings returning their order *)
let string_compare (x : string) (y : string) : order =
  let i = String.compare x y in
  if i = 0 then Equal
  else if i < 0 then Less
  else Greater ;;

(* int_compare x y -- compares two strings returning their order *)
let int_compare (x : int) (y : int) =
  if x = y then Equal
  else if x < y then Less
  else Greater ;;

(* A signature for a module that defines a type and how to compare
   values of that type, as well as ways of generating values of that
   type. *)
module type COMPARABLE =
  sig
    type t
    val compare : t -> t -> order
    val to_string : t -> string

    (* See the tests.ml file for an explanation of what these
       "generate*" functions do, and why we included them in this
       signature. *)

    (* Generate a value of type `t` *)
    val generate : unit -> t

    (* Generate a value of type `t` that is greater than the argument. *)
    val generate_gt : t -> t

    (* Generate a value of type `t` that is less than the argument. *)
    val generate_lt : t -> t

    (* Generate a value of type `t` that is between argument 1 and
       argument 2.  Returns `None` if there is no value between argument
       1 and argument 2. *)
    val generate_between : t -> t -> t option
  end

(* An example implementation of the `COMPARABLE` signature, where the
   underlying type is integers. In this example, the value of the
   integer is used for comparisons. *)
module IntCompare : (COMPARABLE with type t = int) =
  struct
    type t = int

    let compare x y =
      if x < y then Less
      else if x > y then Greater
      else Equal

    let to_string = string_of_int

    let generate () = 0

    let generate_gt x = x + 1

    let generate_lt x = x - 1

    let generate_between x y =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
  end

(* Another example implementation for `int * string` pairs. It only
   uses the `int` part of the tuple in comparisons. *)
module IntStringCompare : (COMPARABLE with type t = int * string) =
  struct
    type t = int * string
                     
    let compare (p1, _) (p2, _) =
      if p1 < p2 then Less
      else if p1 > p2 then Greater
      else Equal

    let to_string (p, s) = "(" ^ string_of_int p ^ ", " ^ s ^ ")"

    let () = Random.self_init ()

    let generate () = (0, 
                       string_of_int 
                         (Random.int (int_of_float (2. ** 30. -. 2.))))

    let generate_gt (p, s) = (p + 1, s ^ " ++")

    let generate_lt (p, s) = (p - 1, s ^ " --")

    let generate_between (p1, _) (p2, s2) =
      let (lower, higher) = (min p1 p2, max p1 p2) in
      (* Reuse the string from the second argument in the output value *)
      if higher - lower < 2 then None else Some (generate_lt (p2, s2))
  end
