(* 
                         CS 51 Problem Set 1
                     Core Functional Programming
 *)

val nonincreasing : int list -> bool
val merge : int list -> int list -> int list
val unzip : (bool * bool) list -> bool list * bool list
val to_run_length : char list -> (int * char) list 
val from_run_length : (int * char) list -> char list 

type action = bool
type play = action * action
type payoff_matrix = (play * (int * int)) list
type history = play list
type strategy = history -> action

val cCOOPERATE : action
val cDEFECT : action
val test_payoff_matrix : payoff_matrix
val nasty : strategy
val patsy : strategy

val extract_entry : play -> payoff_matrix -> int * int
val count_defections : history -> int * int
val count_cooperations : history -> int * int
val balanced : strategy
val egalitarian : strategy
val tit_for_tat : strategy
val my_strategy : strategy
val my_pseudonym : string
val swap_actions : history -> history
val calculate_payoff : payoff_matrix -> history -> int * int
val play_strategies : int -> payoff_matrix -> strategy -> strategy -> int * int

val minutes_spent_on_pset : unit -> int
val reflection : unit -> string
