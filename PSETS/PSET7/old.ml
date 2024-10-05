(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)



(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)

(* Returns true if a mlist element "target" is "lst", false otherwise *)
let rec mmember (lst : 'a mlist) (target : ('a mlist) ref) : bool  =
  match lst with
  | Nil -> false
  | Cons(_, next) -> if (next == target) then true
                     else mmember !next target

(* Returns true if the argument has a cicle *)
let rec has_cycle (lst : 'a mlist) : bool =
  match lst with
  | Nil -> false
  | Cons(_, next) -> match !next with
                     | Nil -> false
                     (* if next is a member of !next2 we have a cycle*)
                     | Cons(_, next2) -> (mmember !next2 next) || has_cycle !next2
