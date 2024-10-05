(* All the parts are now in place to run an iterated prisoners
dilemma. We've provided a function below to do just that. Notice that
it makes good use of your `calculate_payoff` and `swap_actions`
functions. *)
   
(* play_strategies n payoff_matrix strat1 strat2 -- Plays strategies
   `strat1` and `strat2` against each other for `n` rounds, returning
   the cumulative payoffs for both strategies based on the provided
   payoff_matrix`. *)
let play_strategies (n : int)
                    (payoff_matrix : payoff_matrix)
                    (first_strat : strategy)
                    (second_strat : strategy)
                  : int * int =

  (* extend_history past n -- Returns a history that starts with
     `past` and extends it by `n` more plays. *)
  let rec extend_history (past : history) (count : int) : history = 
    if count = 0 then past 
    else 
      let first_action, second_action =
        first_strat past, second_strat (swap_actions past) in
      let new_history = (first_action, second_action) :: past in
      extend_history new_history (count - 1) in
  
  calculate_payoff payoff_matrix (extend_history [] n) ;;

(* Now we can test it out. We'll play Nasty versus Patsy for 100 rounds
and print out the result. To see this, uncomment the single line below
and then compile the file by running `make ps1` in your shell,
followed by the command `./ps1.byte`. Feel free to try out other
strategies by changing `first_strategy` and `second_strategy`
below. But make sure to recomment that last line before submitting
your problem set for grading. *)
  
let cROUNDS = 100 ;;
let first_strategy = nasty ;;
let second_strategy = patsy ;;

let main () = 
  let first_payoff, second_payoff =
    play_strategies cROUNDS test_payoff_matrix first_strategy second_strategy 
  in
  Printf.printf "first payoff: %d, second payoff: %d\n"
                first_payoff second_payoff ;;

let _ = main () ;;

