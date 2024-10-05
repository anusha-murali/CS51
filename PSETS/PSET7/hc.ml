(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)

(* The type of mutable lists. *)
type 'a mlist = 'a mlist_internal ref
 and 'a mlist_internal = 
  | Nil
  | Cons of 'a * 'a mlist ;;

(* let rec found_in_list (lst : 'a mlist) (element : 'a mlist) : bool  =
  match lst.contents with
  | Nil -> false
  | Cons(_, next) -> if (!next = !element) then true
                     else found_in_list next element


let rec has_cycle (lst : 'a mlist) : bool =
  match lst.contents with
  | Nil -> false
  | Cons(_, item1) -> match item1.contents with
                     | Nil -> false
                     | Cons(_, item2) -> 
                        (found_in_list item2 item1) || has_cycle item2
*)

let rec mmember (lst : 'a mlist) (target : 'a mlist) : bool  =
  match lst.contents with
  | Nil -> false
  | Cons(_, next) -> if (!next = !target) then true
                     else mmember next target

(* Returns true if the argument has a cicle *)
let rec has_cycle (lst : 'a mlist) : bool =
  match lst.contents with
  | Nil -> false
  | Cons(_, next) -> match next.contents with
                     | Nil -> false
                     | Cons(_, next2) -> (mmember next2 next) || has_cycle next2
