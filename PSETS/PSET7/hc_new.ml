

(* The type of mutable lists. *)
type 'a mlist = Nil
              | Cons of 'a * 'a mlist ref

(*......................................................................
Problem 1: Write a function has_cycle that returns whether a mutable
list has a cycle. You may want a recursive helper function. Don't
worry about space usage.
......................................................................*)

let getcurrent (lst: 'a mlist ref) : 'a = 
  match !lst with
  | Nil -> failwith "tried to extract value from an empty list!"
  | Cons (h, _) -> h
;;

  let next_node_non_nil (n: 'a mlist ref): 'a mlist ref =
    match !n with
    | Cons (_, t) ->  t
    | _ -> failwith "Impossible list construct"
  ;;

(*
If a cycle exists, this function returns a tuple containing a true boolean,
the length of the cycle (an integer), the length of the list prior to the
cycle, a reference to the element at which
the cycle starts, and a reference to the predecessor of that element in the 
cycle. If a cycle does not exist, this function returns a tuple
containing a false boolean, -1 "length", the length of the list,
and two references to a Nil mutable list.

A full explanation and proof of correctness of our approach can be found in
the file "cycle_algorithm.pdf" in our Git repo (the LaTeX source is also 
included). Essentially, our approach utilizes a "fast" ref that traverses 
the list by advancing two nodes at a time, and a "slow" ref that traverses 
the list one node at a time. These refs will become physically equal (i.e. 
"point" to the same space in memory) at some time after the start if and 
only if a cycle exists. From there, we calculate the cycle length and use 
the cycle length to deduce the the start of the cycle. We also return the
predecessor (within the cycle) to the start of the cycle, so that we can 
easily flatten the cycle.

Notably, this approach does not require any memory beyond the memory taken 
by the list argument passed in and runs in worst case O(n) time, which
satisfies the demands of Challenge Problem 4.

A note on style: we made the decision to include the signatures of helper
functions in cycle_len_start_helper in order to make them less confusing
*)

let rec cycle_len_start_helper 
                            (lst_head: 'a mlist ref) 
                            (fast: 'a mlist ref)
                            (slow: 'a mlist ref) 
                            (curr_lst_len: int)
                          : (bool * int * int * 'a mlist ref * 'a mlist ref) =

  let rec calculatecyclelen 
                        (n1: 'a mlist ref) 
                        (init_node: 'a mlist ref)
                        (curr_len: int) : int =
    if n1 == init_node then curr_len
    else 
      match !n1 with
      | Cons (_, t) -> calculatecyclelen t init_node (curr_len + 1)
      | _ -> failwith "Impossible list construct"
  in

  let calculatecyclestart (cycle_len: int) =
    let rec gen_ahead (len: int) (curr_loc: 'a mlist ref) : 'a mlist ref =
      if len = 0 then curr_loc
      else 
        let next_loc = next_node_non_nil curr_loc in
        gen_ahead (len - 1) next_loc
    in

    let rec tandem_advance_to_start 
                                (ahead_iter: 'a mlist ref) 
                                (behind_iter: 'a mlist ref) 
                                (ahead_iter_prev: 'a mlist ref) 
                                (pre_cycle_len: int)
                              : 'a mlist ref * 'a mlist ref * int =
      if (next_node_non_nil ahead_iter)==(next_node_non_nil behind_iter) then 
        (ahead_iter, ahead_iter_prev, pre_cycle_len)
      else tandem_advance_to_start (next_node_non_nil ahead_iter) 
        (next_node_non_nil behind_iter) ahead_iter (pre_cycle_len + 1)
    in

    let ahead = gen_ahead cycle_len lst_head in
    let ahead_prev = gen_ahead (cycle_len - 1) lst_head in
    let behind = lst_head in
    tandem_advance_to_start ahead behind ahead_prev 0
  in

  match !fast with
  | Nil -> 
    (false, 0, curr_lst_len, (ref Nil), (ref Nil))
  | Cons (_, t1) -> match !t1 with
    | Nil -> (false, 0, (curr_lst_len + 1), (ref Nil), (ref Nil))
    | Cons (_, t2) ->
      let fast = t2 in
      let slow = next_node_non_nil slow in
      if fast == slow then
        let fast_next = next_node_non_nil fast in
        let cycle_len = calculatecyclelen fast_next fast 1 in
        let (cycle_start, cycle_start_prev, pre_cycle_len) = 
          calculatecyclestart cycle_len in
        (true, cycle_len, pre_cycle_len, cycle_start, cycle_start_prev)
      else
        cycle_len_start_helper lst_head fast slow (curr_lst_len + 2)
;;

let has_cycle (lst : 'a mlist) : bool =
  let lst_ref = ref lst in
  let (w,_,_,_,_) = cycle_len_start_helper lst_ref lst_ref lst_ref 0 in
  w
;;

(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.

A special case is needed to deal with the case of a single node whose
successor is itself. In this case, the head of lst (which is not passed
in as a ref) cannot be directly changed, so we change it by setting its tail
reference to refer to Nil.
......................................................................*)
let flatten (lst : 'a mlist) : unit =
  let lst_ref = ref lst in
  let (has_cycle, cycle_len, pre_cycle_len, _, last_node) = 
    cycle_len_start_helper lst_ref lst_ref lst_ref 0 in

  if has_cycle then 
    if(cycle_len + pre_cycle_len = 1) then 
      (match lst with 
       | Nil -> failwith "impossible list construct: cycle with Nil"
       | Cons(_,t) -> t:= Nil; ())
    else                                                           
      match !last_node with
      | Nil -> failwith "Impossible list: cycle with Nil"
      | Cons(h,_) -> last_node := Cons(h, ref Nil); ()
  else ();;

(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)
let mlength (lst : 'a mlist) : int = 
  let lst_ref = ref lst in 
  let (_, cycle_len, pre_cycle_len, _, _) =
    cycle_len_start_helper lst_ref lst_ref lst_ref 0 in
  pre_cycle_len + cycle_len
;;
  
                         
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 320 ;;
