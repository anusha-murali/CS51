(*
                         CS 51 Problem Set 1
                Core Functional Programming -- Testing
*)                           

open Ps1 ;;

(* The Absbook module contains simple functions for unit testing:
   `unit_test` and `unit_test_within`. *)
open CS51Utils ;;
open Absbook ;;
  
let nonincreasing_test () =
  unit_test (nonincreasing [])
            "nonincreasing empty";
  unit_test (nonincreasing [7])
            "nonincreasing single";
  unit_test (nonincreasing [4; 4; 4])
            "nonincreasing repeat";
  unit_test (not (nonincreasing [2; 1; 2]))
            "nonincreasing inc at start";
  unit_test (nonincreasing [2; 2; 1])
            "nonincreasing dups";
  unit_test (nonincreasing [9; 8; 7; 6; 5; 5; 5; 4; 4; ~-2])
            "nonincreasing long with neg";
  unit_test (not (nonincreasing [9; 8; 7; 6; 7; 5; 5; 5; 5; 4; 3]))
            "nonincreasing long inc at mid" ;;


let merge_test () =
  unit_test (merge [] [])
            "merge empty lists";
  unit_test (merge [] [1; 3; 5])
            "merge one empty list with a non-empty list";
  unit_test (merge [1;13] [])
            "merge first list is non-empty with an empty list";
  unit_test (merge [1;3;5] [2;7;15])
            "merge two sorted lists";
  unit_test (merge [1] [15; 20])
            "merge a single element list with another";
  unit_test (merge [1] [2])
            "merge two single element lists";
  unit_test (merge [15; 17] [5; 15; 17; 39])
            "merge a list with identical elements";
  unit_test (merge [-3; 10] [1; 4; 8; 12])
            "merge a list with negative number";
  unit_test (merge [-3; 10] [-7; 1; 4; 8; 12])
            "merge two lists containing negative numbers";;


let unzip_test () =
  unit_test (unzip [(true, false); (false, false); (true, false)])
            "unzip a non-empty boolean pairs list";
  unit_test (unzip [])
            "unzip an empty list";
  unit_test (unzip [(true, true); (true, true); (true, true), (true, true)])
            "unzip boolean pairs containing identical values";;


let to_run_length_test () = 
  unit_test (to_run_length ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'])
            "to_run_length a non-empty list";
  unit_test (to_run_length [])
            "to_run_length an empty list";
  unit_test (to_run_length [a'; 'a'; 'a'; 'a'; 'a'])
            "to_run_length a list with one unique char";;
 
let from_run_length_test () =
  unit_test (from_run_length [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')])
            "from_run_length a number of chars with many occurrences";
  unit_test (from_run_length [])
            "from_run_length with an empty list";
  unit_test (from_run_length [(10, 'a')])
            "from_run_length with just one char repeated many times";;

let extract_entry_test () = 
  unit_test (extract_entry (cDEFECT, cDEFECT) test_payoff_matrix)
            "extract_entry_test with a valid value";
  unit_test (extract_entry (cDEFECT, cCOOPERATE) test_payoff_matrix)
            "extract_entry_test with a valid value";;

let count_defections_test () =
  unit_test (count_defections [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)])
            "opponent has more defections than me";
  unit_test (count_defections [])
            "empty history"
  unit_test (count_defections [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cDEFECT, cCOOPERATE)])
            "I have more defections than the opponent";;

let count_cooperations_test () =
  unit_test (count_cooperations [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)])
            "I have more defections than my opponent";
  unit_test (count_cooperations [])
            "empty history"
  unit_test (count_cooperations [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cDEFECT, cCOOPERATE)])
            "I have less cooperations than the opponent";;

let balanced_test () =
  unit_test (balanced [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cDEFECT, cCOOPERATE)])
            "balanced test wih 4 data points in history";
  unit_test (balanced [])
            "balanced test with empty history";
  unit_test (balanced [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)])
            "balanced test with 2 data points in history";;

let egalitarian_test () =
  unit_test (egalitarian [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cDEFECT, cCOOPERATE)])
            "egalitarian test wih 4 data points in history";
  unit_test (egalitarian [])
            "egalitarian test with empty history";
  unit_test (egalitarian [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)])
            "egalitarian test with 2 data points in history";;

let tit_for_tat_test () =
  unit_test (tit_for_tat [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cDEFECT, cCOOPERATE)])
            "tit_for_tat test wih 4 data points in history";
  unit_test (tit_for_tat [])
            "tit_for_tat test with empty history";
  unit_test (tit_for_tat [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)])
            "tit_for_tat test with 2 data points in history";;

let my_strategy_test () =
  unit_test (my_strategy [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cDEFECT, cCOOPERATE)])
            "my_strategy test wih 4 data points in history";
  unit_test (my_strategy [])
            "my_strategy test with empty history";
  unit_test (my_strategy [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)])
            "my_strategy test with 2 data points in history";;

let swap_actions_test () =
  unit_test (swap_actions [])
            "swap_actions_test empty list";
  unit_test (swap_actions [(cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE)])
            "swap_actions_test with two tupples";;
 
let calculate_payoff_test () = 
  unit_test (calculate_payoff test_payoff_matrix [])
            "calculate_payoff_test empty history";
  unit_test (calculate_payoff test_payoff_matrix [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cDEFECT, cCOOPERATE)])
            "calculate_payoff_test with four elements in history";
  unit_test (calculate_payoff test_payoff_matrix [(cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE)])
            "calculae_payoff_test with two elements in history";;
 
let test_all () =
  nonincreasing_test () ;
  merge_test () ;
  unzip_test () ;
  to_run_length_test ();
  from_run_length_test ();
  extract_entry_test ();
  count_defections_test ();
  count_cooperations_test ();
  balanced_test () ;
  egalitarian_test ();
  tit_for_tat_test ();
  my_strategy_test ();
  swap_actions_test ()
  calculate_payoff_test () ;;

let _ = test_all () ;;
