(* 
                         CS 51 Problem Set 2
                 Higher-Order Functional Programming
 *)

val negate_all : int list -> int list
val sum : int list -> int
val sum_rows : int list list -> int list
val filter_odd : int list -> int list
val num_occurs : int -> int list -> int
val super_sum : int list list -> int
val filter_range : int list -> (int * int) -> int list
val floats_of_ints : int list -> float list
val log10s : float list -> float option list
val deoptionalize : 'a option list -> 'a list
val some_sum : int option list -> int
val mult_odds : int list -> int 
val concat : 'a list list -> 'a list 

type name = string
type year = int
type student = name * year

val filter_by_year : student list -> year -> name list

val minutes_spent_on_pset : unit -> int
val reflection : unit -> string
