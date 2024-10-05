(* 
                         CS 51 Problem Set 4
                 A Language for Symbolic Mathematics
                               Testing
 *)

open Expression ;;
open ExpressionLibrary ;;

open CS51Utils ;; 
open Absbook ;;

let test () =

  (* Unit tests for contains_var *)
  (*******************************)

  unit_test (contains_var (parse "x+3")) "contains_var sum left";
  unit_test (not (contains_var (parse "2"))) "contains_var number";
  unit_test (contains_var (parse "x^4")) "contains_var exponent";
  unit_test (not (contains_var (parse "4+3"))) "contains_var number addition";
  unit_test (not (contains_var (parse "100"))) "contains_var integer";
  unit_test (contains_var (parse "x^3 + 200*x + 1")) "contains_var polynomial";
  unit_test (contains_var (parse "sin(x) + 3")) "contains_var trigonometic function";
  unit_test (not (contains_var (parse "sin(60) + 3"))) "contains_var sin of a number";
  unit_test (contains_var (parse "123 - cos(x^2) + ln(100*x)/(25*x^3) + sin(60) + 3")) 
            "contains_var arbitrary expression";
  unit_test (not (contains_var (parse "123 - cos(15^2) + ln(100*200)/(25*10^3) + sin(60)"))) 
            "contains_var arbitrary expression with only numbers";

 (* Unit tests for evaluate *)
 (***************************)

  unit_test (evaluate (parse "100.0") 0.0 = 100.0 ) "evaluate a number";
  unit_test (evaluate (parse "x") 0.0 = 0.0 ) "evaluate x at zero";
  unit_test (evaluate (parse "~x + 5") 10.0 = -5.0 ) "evaluate a simple expression";
  unit_test (evaluate (parse "x^2 + x^10 + 1000.") 2. = 2028. ) "evaluate a polynomial";
  unit_test (evaluate (parse "sin(3*x)") 0. = 0. ) "evaluate a trigonometric function";
  unit_test (evaluate (parse "(ln x)/3") 1000.0 = 1.0 ) "evaluate log of x";
  unit_test (evaluate (parse "cos(3*x)") 0. = 1. ) "evaluate a trigonometric function";
  unit_test (evaluate (parse "ln x") 100000000000000000.0 = 17.0 ) 
            "evaluate ln of a large number";

  (* Unit tests for derivative *)
  (*****************************)

  unit_test (derivative (parse "100.0") = Num 0.0 ) "derivative of a constant";
  unit_test (to_string_smart (derivative (parse "10*x")) = "10.*1.+0.*x" ) 
            "derivative of a simple expression";
  unit_test (to_string_smart (derivative (parse "x^2 + 5")) = "2.*1.*x^(2.-1.)+0." )
            "derivative of a quadratic";
  unit_test (to_string_smart (derivative (parse "sin(x) - cos(x)")) = "cos(x)*1.-~(sin(x))*1." )
            "derivative of a trigonometric expression";
  unit_test (to_string_smart (derivative (parse "~sin(x) + cos(x)")) = "~(cos(x)*1.)+~(sin(x))*1." )
            "derivative of a trigonometric expression";
  unit_test (to_string_smart (derivative (parse "sin(x) - cos(x^2)")) = "cos(x)*1.-~(sin(x^2.))*2.*1.*x^(2.-1.)" )
            "derivative of a trigonometric expression";
  unit_test (to_string_smart (derivative (parse "(x^2 + 3)*(x-2)")) = "(x^2.+3.)*(1.-0.)+(2.*1.*x^(2.-1.)+0.)*(x-2.)" )
            "derivative of an arbitrary polynomial";
  unit_test (to_string_smart (derivative (parse "(12*x^5 - 3*x^2 + 23)/(7*x^3 + 2*x +10)")) = "(((7.*x^3.+2.*x)+10.)*(((12.*5.*1.*x^(5.-1.)+0.*x^5.)-(3.*2.*1.*x^(2.-1.)+0.*x^2.))+0.)-((12.*x^5.-3.*x^2.)+23.)*(((7.*3.*1.*x^(3.-1.)+0.*x^3.)+2.*1.+0.*x)+0.))/(((7.*x^3.+2.*x)+10.)*((7.*x^3.+2.*x)+10.))" )
            "derivative of a complicated algebraic expression";
  unit_test (to_string_smart (derivative (parse "(5*x^2 + 10*x)^3")) = "3.*((5.*2.*1.*x^(2.-1.)+0.*x^2.)+10.*1.+0.*x)*(5.*x^2.+10.*x)^(3.-1.)" )
            "derivative of a polynomial with nested exponents";

  (* Unit tests for find_zero *)
  (****************************)
  
  unit_test (find_zero (parse "x - 1") 0.5 0.00001 100 = Some 1. ) 
            "find_zero of a simple equation";
  unit_test (find_zero (parse "x^2 - 9") 0.5 0.00001 100 = Some 3.0000000026665945 )
            "find_zero of a quadratic equation";
  unit_test (find_zero (parse "3*x - 1") 0. 0.00001 100 = Some 0.333333333333333315 )
            "find_zero of a simple equation";
  unit_test (find_zero (parse "3*x^2 + 2*x + 120") 0. 0.00001 100 = None )
            "find_zero of a quadratic with complex solutions";
  unit_test (find_zero (parse "sin(x^2) - 1") 0.5 0.00001 100 = Some 1.25480347532752456 )
            "find_zero of a trignometric equation";
  unit_test (find_zero (parse "ln(x) + sin(x^3) - cos(x)") 0.5 0.00001 100 = Some 0.900402120070868173 )
            "find_zero of an arbitrary equation involving log and trigonometric functions";
  unit_test (find_zero (parse "(12*x^7 + 13*x)^3") 0.5 0.00001 100 = Some 0.00119288747910505262 )
            "find_zero of an arbitrary polynomial involving large powers";
unit_test ( find_zero (parse "ln(12*x^7 + 13*x)^3") 0.5 0.00001 100 = Some 0.0804236652995541923 )
            "find_zero of an arbitrary polynomial with large powers involving log";

  () ;;

let _ = test () ;;

