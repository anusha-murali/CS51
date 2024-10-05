(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                         Native Lazy Streams
                             Spring 2017
 *)

(*......................................................................
A native implementation of lazy streams with some useful functions and
applications.

		    YOU SHOULD NOT EDIT THIS FILE.
 *)

type 'a str = Cons of 'a * 'a stream
 and 'a stream = 'a str Lazy.t ;;
  
let head (s : 'a stream) : 'a =
  let Cons (h, _) = Lazy.force s in h ;;
  
let tail (s : 'a stream) : 'a stream =
  let Cons (_, t) = Lazy.force s in t ;;
  
let rec first (n : int) (s : 'a stream) : 'a list =
  if n = 0 then []
  else head s :: first (n - 1) (tail s) ;;
 
let rec smap (f : 'a -> 'b)
             (s : 'a stream)
           : ('b stream) = 
  lazy (Cons (f (head s),
	      smap f (tail s))) ;;
   
let rec smap2 (f : 'a -> 'b -> 'c)
              (s1 : 'a stream)
              (s2 : 'b stream)
            : 'c stream = 
  lazy (Cons (f (head s1) (head s2), 
             smap2 f (tail s1) (tail s2))) ;;

(*......................................................................
Some useful streams and examples.
 *)

(* An infinite sequence of ones *)
let rec ones =
  lazy (Cons(1, ones)) ;;  

(* The natural numbers *)
let rec nats =
  lazy (Cons(0, smap ((+) 1) nats)) ;;
   
(* The Fibonnaci sequence: 0, 1, 1, 2, 3, 5, 8, 13, ... *)
let rec fibs =
  lazy (Cons(0, lazy (Cons(1, smap2 (+) fibs (tail fibs))))) ;;
  
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

let neg_evens = smap (fun x -> if x mod 2 = 0 then -x else x) ;;

(* 1, -1, 1, -1, ... *)
let alt_signs = smap2 ( / ) (neg_evens (tail nats)) (tail nats) ;;

(* 1, -1/3, 1/5, -1/7, ... *)
let pi_stream = smap2 ( /. )
                      (to_float (smap (( * ) 4) alt_signs))
                      (to_float odds) ;;

let pi_approx n =
  List.fold_left ( +. ) 0.0 (first n pi_stream) ;;

let rec sums s =
  smap2 ( +. ) s (lazy (Cons(0.0, sums s))) ;;

let pi_sums = sums pi_stream ;;
  
(* within -- Return the index and the value of the first element in
   the stream s to be within eps of the following element. *)
let within (eps : float) (s : float stream) : (int * float) =
  let rec within' steps s =
    let (h, t) = (head s, tail s) in
    if abs_float (h -. (head t)) < eps then (steps, h)
    else within' (steps+1) t in
  within' 0 s ;;
