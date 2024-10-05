(*
                         CS 51 Problem Set 4
                 A language for symbolic mathematics
 *)

(*======================================================================
Before working on this problem set, read the problem set 4 writeup in
`readme.pdf`. It provides context and crucial information for
completing the problems. In addition, make sure that you are familiar
with the problem set procedures in the document "Problem set
procedures for CS51".

We provide a type definition whose values represent arithmetic
expressions over floating point numbers with a single variable. The
type definition can be found at the top of `expressionLibrary.ml`,
along with enumerated type definitions for the unary and binary
operators, and other useful functions. You will be using this
algebraic data type for this part of the problem set. We refer to
these arithmetic expressions (represented by the `expression` type) as
the "object language". It is the object of our interest, the object of
our software's manipulation, as distinct from the "metalanguage"
(OCaml) in which our manipulations are written.

The module `ExpressionLibrary` is opened here to provide you with
access to the `expression` data type and helpful functions that you
will use for this part of the problem set.
......................................................................*)

open ExpressionLibrary ;;

(*......................................................................
Tips:

1. READ THE WRITEUP, particularly for the definition of the derivative
   function.
 
2. Use the type definitions provided at the top of
   `expressionLibrary.ml` as a reference, and don't change any of the
   code in that file. It provides functions such as `parse` and
   `to_string_smart` that will be helpful in this problem set.
......................................................................*)

(*......................................................................
Problem 1: Implement the function `contains_var : expression -> bool`,
which tests whether an expression contains a variable `x`. For
example:

# contains_var (parse "x^4") ;;
- : bool = true
# contains_var (parse "4+3") ;;
- : bool = false
......................................................................*)

let rec contains_var (e : expression) : bool =
  match e with
  | Var -> true
  | Num _ -> false
  | Binop (_, p, q) -> contains_var p || contains_var q
  | Unop (_, p) -> contains_var p
;;


(* Unit tests for Problem 1 *)

assert( contains_var (parse "x^4") = true );;
assert( contains_var (parse "4+3") = false );;
assert( contains_var (parse "100") = false );;
assert( contains_var (parse "x^3 + 200*x + 1") = true );;
assert( contains_var (parse "sin(x) + 3") = true );;
assert( contains_var (parse "sin(60) + 3") = false );;
assert( contains_var (parse "123 - cos(x^2) + ln(100*x)/(25*x^3) + sin(60) + 3") = true );;
assert( contains_var (parse "123 - cos(15^2) + ln(100*200)/(25*10^3) + sin(60)") = false );;

(*......................................................................
Problem 2: Implement the function `evaluate : expression -> float ->
float`, which evaluates an expression at a particular value of
`x`. Don't worry about specially handling the "divide by zero"
case. For example, evaluating the expression x^4 + 4, where x is 2,

# evaluate (parse "x^4 + 3") 2.0
- : float = 19.0
......................................................................*)

let rec evaluate (e : expression) (x : float) : float =
   match e with
   | Var -> x
   | Num n -> n
   | Binop (op, p, q) ->
     (match op with
      | Add -> (evaluate p x) +. (evaluate q x)
      | Sub -> (evaluate p x) -. (evaluate q x)
      | Mul -> (evaluate p x) *. (evaluate q x)
      | Div -> (evaluate p x) /. (evaluate q x)
      | Pow -> (evaluate p x) ** (evaluate q x)
     )
   | Unop (op, p) ->
     (match op with
      | Sin -> (sin) (evaluate p x)
      | Cos -> (cos) (evaluate p x)
      | Ln  -> (log10) (evaluate p x)
      | Neg -> (~-.) (evaluate p x)
     ) 
;;

(* Unit tests for Problem 2 *)

assert( evaluate (parse "100.0") 0.0 = 100.0 );;
assert( evaluate (parse "x") 0.0 = 0.0 );;
assert( evaluate (parse "~x + 5") 10.0 = -5.0 );;
assert( evaluate (parse "x^2 + x^10 + 1000.") 2. = 2028. );;
assert( evaluate (parse "sin(3*x)") 0. = 0. );;
assert( evaluate (parse "(ln x)/3") 1000.0 = 1.0 );;
assert( evaluate (parse "cos(3*x)") 0. = 1. );;
assert( evaluate (parse "ln x") 100000000000000000.0 = 17.0 );;

(*......................................................................
Problem 3: Implement the `derivative : expression -> expression`
function, which returns an expression that represents the derivative
of the argument expression. We provide the skeleton of the
implementation here along with a few of the cases; you're responsible
for filling in the remaining parts that implement the derivative
transformation provided in the figure in the writeup. See the writeup
for details.
......................................................................*)

let rec derivative (e : expression) : expression =
  match e with
  | Num _ -> Num 0.
  | Var -> Num 1.
  | Unop (u, e1) ->
     (match u with
      | Sin -> Binop (Mul, Unop (Cos, e1), derivative e1)
      | Cos -> Binop (Mul, Unop (Neg, Unop (Sin, e1)), derivative e1)
      | Ln -> Binop (Mul, Binop (Div, (Num 1.), e1), derivative e1)
      | Neg -> Unop (Neg, derivative e1))
  | Binop (b, e1, e2) ->
     match b with
     | Add -> Binop (Add, derivative e1, derivative e2)
     | Sub -> Binop (Sub, derivative e1, derivative e2)
     | Mul -> Binop (Add, Binop (Mul, e1, derivative e2),
                          Binop (Mul, derivative e1, e2))
     | Div -> Binop (Div, Binop (Sub, (Binop (Mul, e2, (derivative e1))),
                                      (Binop (Mul, e1, (derivative e2)))), 
                          Binop (Mul, e2, e2))
     | Pow -> if (contains_var e2) then
                 Binop (Mul, Binop (Pow, e1, e2), 
                   (Binop (Add, (Binop (Mul, (derivative e2),
                      (Unop (Ln, e1)))),
                 Binop (Div, Binop (Mul, (derivative e1), e2), e1)))) 
               else
                 Binop (Mul, e2, (Binop (Mul, (derivative e1), 
                   Binop (Pow, e1, (Binop (Sub, e2, Num 1.))))))
;;

(* Unit tests for Problem 3 *)
                 
assert( derivative (parse "100.0") = Num 0.0 );;
assert( to_string_smart (derivative (parse "10*x")) = "10.*1.+0.*x" );;
assert( to_string_smart (derivative (parse "x^2 + 5")) = "2.*1.*x^(2.-1.)+0." );;    
assert( to_string_smart (derivative (parse "sin(x) - cos(x)")) = "cos(x)*1.-~(sin(x))*1." );;
assert( to_string_smart (derivative (parse "~sin(x) + cos(x)")) = "~(cos(x)*1.)+~(sin(x))*1." );;
assert( to_string_smart (derivative (parse "sin(x) - cos(x^2)")) = "cos(x)*1.-~(sin(x^2.))*2.*1.*x^(2.-1.)" );;
assert( to_string_smart (derivative (parse "(x^2 + 3)*(x-2)")) = "(x^2.+3.)*(1.-0.)+(2.*1.*x^(2.-1.)+0.)*(x-2.)" );;
assert( to_string_smart (derivative (parse "(12*x^5 - 3*x^2 + 23)/(7*x^3 + 2*x +10)")) = "(((7.*x^3.+2.*x)+10.)*(((12.*5.*1.*x^(5.-1.)+0.*x^5.)-(3.*2.*1.*x^(2.-1.)+0.*x^2.))+0.)-((12.*x^5.-3.*x^2.)+23.)*(((7.*3.*1.*x^(3.-1.)+0.*x^3.)+2.*1.+0.*x)+0.))/(((7.*x^3.+2.*x)+10.)*((7.*x^3.+2.*x)+10.))" );;
assert( to_string_smart (derivative (parse "(5*x^2 + 10*x)^3")) = "3.*((5.*2.*1.*x^(2.-1.)+0.*x^2.)+10.*1.+0.*x)*(5.*x^2.+10.*x)^(3.-1.)" );;


 
(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
  print_string ("Checking expression: " ^ strs ^ "\n");
  let parsed = parse strs in
  (print_string "contains variable : ";
   print_string (string_of_bool (contains_var parsed));
   print_endline " ";
   print_string "Result of evaluation: ";
   print_float (evaluate parsed xval);
   print_endline " ";
   print_string "Result of derivative: ";
   print_endline " ";
   print_string (to_string (derivative parsed));
   print_endline " ") ;;
  
(*......................................................................
Problem 4: Zero-finding. See writeup for instructions.
......................................................................*)

let find_zero (expr : expression)
              (guess : float)
              (epsilon : float)
              (limit : int)
            : float option =
  let rec newtons_method (x_n : float) (n : int) = 
    if abs_float(evaluate expr x_n) < epsilon then Some x_n else 
    if n >= limit then None else
      newtons_method 
        (x_n -. ((evaluate expr x_n) /. (evaluate (derivative expr) x_n))) 
        (n + 1)
    in
      newtons_method guess 0
;;


(* Unit tests for Problem 4 *)

assert( find_zero (parse "x - 1") 0.5 0.00001 100 = Some 1. );;
assert( find_zero (parse "x^2 - 9") 0.5 0.00001 100 = Some 3.0000000026665945 );;
assert( find_zero (parse "3*x - 1") 0. 0.00001 100 = Some 0.333333333333333315 );;
assert( find_zero (parse "3*x^2 + 2*x + 120") 0. 0.00001 100 = None );;
assert( find_zero (parse "sin(x^2) - 1") 0.5 0.00001 100 = Some 1.25480347532752456 );;
assert( find_zero (parse "ln(x) + sin(x^3) - cos(x)") 0.5 0.00001 100 = Some 0.900402120070868173 );;
assert( find_zero (parse "(12*x^7 + 13*x)^3") 0.5 0.00001 100 = Some 0.00119288747910505262 );;
assert( find_zero (parse "ln(12*x^7 + 13*x)^3") 0.5 0.00001 100 = Some 0.0804236652995541923 );;

(*......................................................................
Problem 5: Challenge problem -- exact zero-finding. This problem is
not counted for credit and is not required. Just leave it
unimplemented if you do not want to do it. See writeup for
instructions.
......................................................................*)

let find_zero_exact (e : expression) : expression option =
  failwith "find_zero_exact not implemented" ;;

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set took you to complete. 
......................................................................*)

let minutes_spent_on_pset () : int =
  failwith "time estimate not provided" ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set. Where
did you run into problems and how did you end up resolving them? What
might you have done in retrospect that would have allowed you to
generate as good a submission in less time? Please provide us your
thoughts on these questions and any other reflections in the string
below.
......................................................................*)

let reflection () : string =
  "...your reflections here..." ;;
