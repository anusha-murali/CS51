(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                          General utilities
 *)

open Config ;;

(* sum_float float_list -- Returns the sum of the elements in the
   `float_list`. *)
let sum_float : float list -> float = List.fold_left (+.) 0. ;;

(* sum_int int_list -- Similarly for `int`s. *)
let sum_int : int list -> int = List.fold_left (+) 0 ;;

(* bounded n min max -- Returns `n` (or thereabouts), but clipped
   between `min` (inclusive) and `max` (exclusive) *)
let bounded (n : int) (minimum : int) (maximum : int) : int =
  min (pred maximum) (max minimum n) ;;

(* x_bounded, y_bounded -- Versions specifically to clip `x` and `y`
   coordinates *)
let x_bounded (n : int) =
  bounded n 0 cX_DIMENSION ;;

let y_bounded (n : int) =
  bounded n 0 cY_DIMENSION ;;

(* gaussian mean stdev -- Returns a random `float` sampled from a
   Gaussian distribution with the given `mean` and `stdev`. If you
   need a random `int`, this function can (and should) be used for
   sampling of `int`s using the `Stdlib.truncate` or
   `Stdlib.int_of_float` functions on the output, or perhaps using the
   various truncation and rounding operations in the `Float` library
   module, q.v. *)
let gaussian (mean : float) (stdev : float) : float =
  (* generated using the Box-Muller method *)
  let u = Random.float 1.0 in
  let v = Random.float 1.0 in
  let z = sqrt (~-.2. *. log u) *. cos (2. *. Float.pi *. v) in
  mean +. stdev *. z ;;

(* flip_coin probability -- Returns `true` with the given
   `probability`. *)
let flip_coin (probability : float) : bool =
  Random.float 1.0 < probability ;;

(* rand_step x y step_size -- Returns new `x, y` pair taking a random
   step at most `step_size` units, clipped to the frame size *)
let rand_step (x : int) (y : int) (step_size : int) : int * int =
  let width = 2 * step_size + 1 in
  let delta_x, delta_y =
    Random.int width - step_size, Random.int width - step_size in
  x_bounded (x + delta_x), y_bounded (y + delta_y) ;;

(* cross_product lst1 lst2 -- Returns a list containing all pairs of
   elements from `lst1` and `lst2`. For example:

      # cross_product [1; 2; 3] [true; false] ;;
      - : (int * bool) list =
      [(3, false); (3, true); (2, false); (2, true); (1, false); (1, true)]
 *)
let cross_product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  l1
  |> List.fold_left
       (fun acc1 ele1 -> l2
                         |> List.fold_left
                              (fun acc2 ele2 -> (ele1, ele2) :: acc2)
                              acc1)
       [] ;;

(* gensym () -- Returns a numbered string of the form `"xnnn"` where
   `nnn` is a unique integer. *)
let gensym : unit -> string =
  let ctr = ref 0 in
  fun () -> incr ctr;
            "x" ^ (string_of_int !ctr) ;;
