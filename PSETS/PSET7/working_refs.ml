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

(*......................................................................
Problem 1: Write a function `has_cycle` that returns `true` if a
mutable list has a cycle, `false` otherwise. You may want a recursive
auxiliary function. You needn't worry about space usage of your
function.

For instance, we can establish a cyclic and an acyclic mutable list
like this:

    # let sample_end = ref Nil ;;
    # let cyclic = ref (Cons (1, ref (Cons (2, sample_end)))) ;;
    # sample_end := !cyclic ;;
    # let acyclic = ref (Cons (3, ref (Cons(4, ref Nil)))) ;;

and test for cycles using `has_cycle`:

    # has_cycle cyclic ;;
    - : bool = true
    # has_cycle acyclic ;;
    - : bool = false
......................................................................*)
                                
(* This helper function returns true if the input element is found in
   the mlist *)
let rec found_in_list (lst : 'a mlist) (element : 'a mlist) : bool  =
  match lst.contents with
  | Nil -> false
  | Cons(_, next) -> if (next == element) then true
                     else found_in_list next element


(* let rec has_cycle (lst : 'a mlist) : bool =
  (* We take an element in the list and compare with each element down
     the list to see if their memory locations are the same. If two
     elements in the list share the same memory locations, then we have
     found a cycle *)
  match lst.contents with
  | Nil -> false
  | Cons(_, item1) -> match item1.contents with
                     | Nil -> false
                     | Cons(_, item2) -> 
                        (found_in_list item2 item1) || has_cycle item2

*)

let has_cycle (lst : 'a mlist) : bool =
  (* This helper function returns true if the input element is found in
   the mlist *)
  let rec found_in_list lst' e : bool =
    match lst' with
    |Nil -> false
    |Cons (x, r) -> (match e with
                     |Nil -> false 
                     |Cons (x', r') -> 
                       (match !r' with (* check for cycle *)
                        |Nil -> false 
                        |Cons (x'', r'') ->
                          if (x = x'') && ((!r) == (!r'')) then true
                          else 
                            if (x = x') && ((!r) == (!r')) then
                              found_in_list lst.contents (!r')
                            else found_in_list (!r) e))
  in
  found_in_list lst.contents lst.contents


let has_cycle (lst : 'a mlist) : bool =
  (* This helper function returns true if the input element is found in
   the mlist *)
  let rec found_in_list lst2 item : bool =
    match lst2 with
    |Nil -> false
    |Cons (a1, b1) -> 
          (match item with
             |Nil -> false 
             |Cons (a2, b2) ->
             (match !b2 with (* check for cycle *)
                |Nil -> false 
                |Cons (a3, b3) ->
                   if (a1 = a3) && ((!b1) == (!b3)) 
                     then true
                   else 
                   if (a1 = a2) && ((!b1) == (!b2)) 
                     then found_in_list lst.contents (!b2)
                   else found_in_list (!b1) item))
  in
  found_in_list lst.contents lst.contents


(*......................................................................
Problem 2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
......................................................................*)

(* This helper function sets the tail of lst if it is found to be the
   same as the input item *)
let rec flatten_helper (lst : 'a mlist) (item : 'a mlist) =
  match lst.contents with
  | Nil -> ()
  | Cons(_, tl) -> if (tl == item) then tl := Nil
                    else flatten_helper tl item

(* Removes cycles from an mlist *)
let rec flatten (lst : 'a mlist) : unit =
  match lst.contents with
  | Nil -> ()
  | Cons(_, item1) ->
    (match item1.contents with
     | Nil -> ()
     | Cons(_, item2) -> flatten_helper lst item2; flatten item2)

(*......................................................................
Problem 3: Write a function `mlength`, which nondestructively returns
the number of nodes (that is `Cons`es) in a mutable list that may have
cycles.
......................................................................*)

(*  This helper function returns true if item is found in the lst *)
let rec found_item lst item =
  match lst with
  | [] -> false
  | hd :: tl -> if (hd == item) then true
  else found_item tl item

(* mlength counts the number of nodes in an mlist. It terminates
   when it finds a cycle or when the list ends *)
let mlength (lst : 'a mlist) : int =
  let rec count_nodes new_list n item_lst =
    match new_list with
    | Nil -> n
    | Cons(_,y) -> if (found_item item_lst !y) then n
                   else count_nodes !y (n + 1) (!y :: item_lst) in
  count_nodes lst.contents 0 []
;;


(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete. (If you worked with a partner, we're asking for how much time
each of you (on average) spent on the problem set, not in total.)
......................................................................*)

let minutes_spent_on_pset () : int = 240

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "I very much enjoyed computing pi using various methods." ;;
