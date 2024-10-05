(* 
                         CS 51 Problem Set 2
            Higher Order Functional Programming -- Testing
 *)

open Unix;;

open Mapfold ;;
open CS51Utils ;;
open Absbook ;;
  
let test () =
  unit_test ((negate_all []) = [])
            "negate_all empty";
  unit_test ((negate_all [1; ~-2; 0]) = [~-1; 2; 0])
            "negate_all mixed";
  unit_test ((sum [-3; -4; 5; -90; 0; 12; -12]) = -92)
            "sum positive and negative integers";
  unit_test ((sum [-3; -4; 5; -90; 0; 12; -12; 3;4;-5; 90; 0; -12; 12]) = 0)
            "sum positive and negative integers";
  unit_test ((sum []) = 0)
            "sum an empty list";
  unit_test ((sum [10]) = 10)
            "sum a single element list";
  unit_test ((sum (List.append [-3; -4; 5; -90; 0; 12; -12] (negate_all [-3; -4; 5; -90; 0; 12; -12]))) = 0)
            "sum a list of int with another list of the negatives";
  unit_test ((sum_rows []) = [])
            "sum_rows an empty list with no rows";
  unit_test ((sum_rows [[]]) = [0])
            "sum_rows an empty list with an empty row";
  unit_test ((sum_rows [[1; 2]; [3; 4]]) = [3; 7])
            "sum rows of a list with list of integers"; 
  unit_test ((filter_odd [1; 4; 5; -3]) = [1; 5; -3])
            "filter_odd a mixed list of positives and negatives";
  unit_test ((filter_odd [0]) = [])
            "filter_odd a single even element in the list";
  unit_test ((filter_odd [37]) = [37])
            "filter_odd a single odd element in the list";

  (*  Additional tests go here... *)

  () ;;

test () ;;
