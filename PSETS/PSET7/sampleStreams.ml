(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                            Sample Streams

                    YOU SHOULD NOT EDIT THIS FILE.
 *)

(* open NativeLazyStreams ;; *)
  
(*......................................................................
Some sample useful functions and applications of streams, taken from
Section 17.4 of the textbook.
 *)

(* An infinite sequence of ones *)
let rec ones =
  lazy (Cons (1, ones)) ;;  

(* The natural numbers *)
let rec nats =
  lazy (Cons (0, smap succ nats)) ;;
   
(* The Fibonnaci sequence: 0, 1, 1, 2, 3, 5, 8, 13, ... *)
let rec fibs =
  lazy (Cons (0, lazy (Cons(1, smap2 (+) fibs (tail fibs))))) ;;
  
(* Let's apply these streams to a more interesting problem. A Taylor
series is a representation of a function as an infinite sum of
individual terms. For instance,

    arctan x = x - x^3/3 + x^5/5 - x^7/7 + ...

Setting x to 1, we have

    pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...

This gives us a simple way to approximate pi. *)
      
let to_float = smap float_of_int ;;

(* 1, 3, 5, 7, ... *)
let odds = smap (fun x -> x * 2 + 1) nats ;;

(* 1, -1, 1, -1, ... *)
let alt_signs = smap (fun x -> if x mod 2 = 0 then 1 else ~-1) nats ;;
  
(* 1, -1/3, 1/5, -1/7, ... *)
let pi_stream = smap2 ( /. )
                      (to_float (smap (( * ) 4) alt_signs))
                      (to_float odds) ;;

let pi_approx n =
  List.fold_left ( +. ) 0.0 (first n pi_stream) ;;

let rec sums s =
  smap2 ( +. ) s (lazy (Cons (0.0, sums s))) ;;

let pi_sums = sums pi_stream ;;
  
(* within -- Return the index and the value of the first element in
   the stream s to be within eps of the following element. *)
let within (eps : float) (s : float stream) : (int * float) =
  let rec within' steps s =
    let (h, t) = (head s, tail s) in
    if abs_float (h -. (head t)) < eps then (steps, h)
    else within' (steps + 1) t in
  within' 0 s ;;
