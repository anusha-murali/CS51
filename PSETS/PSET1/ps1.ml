(* 
                         CS 51 Problem Set 1
                        The Prisoners' Dilemma
 *)

(* Before reading this code (or in tandem), read the problem set 1
writeup in the `readme.pdf` file that came along with the problem
set. It provides context and crucial information for completing the
problems. In addition, make sure that you are familiar with the
problem set procedures in the document "Problem set procedures for
CS51" <https://url.cs51.io/pset-procedures>. *)

(*======================================================================
Part 1 - Practice with simple functions

We start with writing a few simple functions as a warm up. These
functions perform simple manipulations over lists, strings, numbers,
and booleans. (Some of them may also be useful later in part 2 in
implementing the iterated prisoner's dilemma.)

  ----------------------------------------------------------------------
  Before proceeding further, please read the document "Problem set
  procedures for CS51". The following recommendations supplement those
  notes.

  * For each subproblem, you will implement a given function, and
    provide appropriate unit tests in the accompanying file
    `pset1_tests.ml`. You are provided a high level description as well
    as a type signature of the function you must implement. 

  * Give the functions the names listed in the comments, as they must
    be named as specified in order to compile against our automated
    unit tests.

  * Keep in mind the CS51 style guide and what you've learned so far
    about code quality and elegance. 

  * The best way to learn about the core language is to work directly
    with the core language features. Consequently, you should *not*
    use any library functions (for instance, those in the `List`
    module) in implementing your solutions to this problem set.

  * We recommend that you specify your unit tests for a function
    _before_ working on writing the function. This development cycle
    --

        (i) Understand the function requirements
       (ii) Create unit tests
      (iii) Implement the function 

    _in that order_ -- is highly recommended for all future work in
    this course and beyond. Following such a development cycle will
    give you a clearer idea of what it is you'll be implementing, and
    helps improve your understanding of the task at hand before
    getting deep into the code, and will hopefully minimize bugs and
    headaches. Unit tests should be your first activity, not an
    afterthought.

    As an example, we've already provided some unit tests in
    `pset1_tests.ml` for the `nonincreasing` function. (Of course, you
    should feel free to add more.)

  * In these problem sets and in the labs as well, we'll often supply
    some skeleton code for functions that you are to write.
    Typically, that "stub" code merely raises an exception noting that
    the function has not yet been written -- something like

      let merge =
        fun _ -> failwith "merge not implemented" ;;

    (The `failwith` function will be explained later in the
    course. For the time being, you can treat this as a fixed idiom.)

    You'll want to replace these stubs with a correct implementation
    of the function. Other parts of the assignment may have you
    perform other tasks to check your understanding of the material.

  * Often, the skeleton code is written to give you an idea of the
    function's intended name and its signature (its type, including
    its arguments and their types, and the return type), for instance,

        let somefunction (arg1 : type) (arg2 : type) : returntype =
          failwith "somefunction not implemented"

    This stub code indicates that `somefunction` takes two arguments
    of the indicated types and returns a value of the indicated return
    type. But there's no need to slavishly follow that particular way
    of *implementing* the function to those specifications. In
    particular, you may want to modify the first line to introduce,
    say, a `rec` keyword (if your function is to be recursive):

        let rec somefunction (arg1 : type) (arg2 : type) : returntype =
          ...your further code here...

    Or you might want to define the function using anonymous function
    syntax:

        let somefunction =
          fun (arg1 : type) (arg2 : type) : returntype ->
            ...your further code here...

    In short, you can and should write these function definitions in
    as idiomatic a form as possible, regardless of how the stub code
    has been organized.
    ------------------------------------------------------------------*)

(*......................................................................
Problem 1a: Write a function `nonincreasing` that takes a list of
integers and returns `true` if and only if the list is in
nonincreasing order. The empty list is considered to be nonincreasing
in this sense. Consecutive elements of the same value are allowed in a
nonincreasing list.

For example:

    # nonincreasing [1; 2; 3] ;;
    - : bool = false
    # nonincreasing [1; 2; 1; 2] ;;
    - : bool = false
    # nonincreasing [3; 2; 1] ;;
    - : bool = true
    # nonincreasing [5; 2; 2; 2; 1; 1] ;;
    - : bool = true

Here is the function's signature: 

    nonincreasing : int list -> bool

Start by writing a full set of unit tests in `ps1_tests.ml` (but we've
done that for you for this problem). Try to cover both typical cases
and "edge cases". Once you have a good set of unit tests, replace the
stub below with your own definition of `nonincreasing`. (Recall the
"important notes" above.) Then compile and run the tests to check that
your function works, before moving on to the next problem.
......................................................................*)

let rec nonincreasing =
  fun (x : int list) ->
  match x with
  | [] -> true
  | _ :: [] -> true
  | h1::h2::t -> if h1 >= h2 then nonincreasing(h2::t) else false;;

(*......................................................................
Problem 1b: Write a function `merge` that takes two integer lists, each
*sorted* in nondecreasing order, and returns a single merged list in
sorted order. For example:

    # merge [1; 3; 5] [2; 4; 6] ;; 
    - : int list = [1; 2; 3; 4; 5; 6]
    # merge [1; 2; 5] [2; 4; 6] ;;
    - : int list = [1; 2; 2; 4; 5; 6]
    # merge [1; 3; 5] [2; 4; 6; 12] ;;
    - : int list = [1; 2; 3; 4; 5; 6; 12]
    # merge [1; 3; 5; 700; 702] [2; 4; 6; 12] ;;
    - : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

Here is its type signature:

    merge : int list -> int list -> int list

As before, you should first provide unit tests, and then work on
writing the function. Replace the stub below with your own definition
of `merge`.
......................................................................*)

let rec merge =
  fun (lst1 : int list) (lst2 : int list) ->
  let rec insert (x : int) (lst : int list) =
  match lst with
  | [] -> [x]
  | h::t -> if x <= h then x::h::t
      else h:: insert x t in
  match lst1 with
  | [] -> lst2
  | h::t -> merge t (insert h lst2);; 

(*......................................................................
Problem 1c: The function `unzip`, given a list of boolean pairs,
returns a pair of lists, the first of which contains each first
element of each pair, and the second of which contains each second
element. The returned list should have elements in the order in which
they were provided. For example:

    # unzip [(true, false); (false, false); (true, false)] ;;
    - : bool list * bool list = ([true; false; true], [false; false; false])

Here is its signature:

    unzip : (bool * bool) list -> bool list * bool list)

As before, you should first provide unit tests, and then work on
writing the function. (We'll stop reminding you about writing the unit
tests first, not because it's not important but because it ought to go
without saying.) Replace the stub below with your own definition of
`unzip`.
......................................................................*)

let rec unzip =
  fun (lst : (bool * bool) list) : (bool list * bool list)  ->
  match lst with
  | [] -> ([], [])
  | [(x, y)] -> ([x], [y])
  | (x, y) :: t ->
      let (a, b) = unzip t in (x :: a, y :: b);; 

(*......................................................................
Problem 1d: One way to compress a list of characters is to use
run-length encoding. The basic idea is that whenever we have repeated
characters in a list such as

  ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'] 

we can represent the same information more compactly (usually) as a
list of pairs like 

  [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]      . 

Here, the numbers represent how many times the character is
repeated. For example, the first character in the string is 'a' and it
is repeated 5 times in a row, followed by 3 occurrences of the
character 'b', followed by 1 more 'a', and finally 4 copies of 'd'.

Write a function `to_run_length` that converts a list of characters
into the run-length encoding and a function `from_run_length` that
converts back. Writing both functions will make it easier to test that
you've gotten them right.

Here are their signatures:

  to_run_length : char list -> (int * char) list
  from_run_length : (int * char) list -> char list

Replace the stubs below with your own definitions of `to_run_length`
and `from_run_length`. We recommend that you write `from_run_length`
first, and then `to_run_length`.
......................................................................*)

let rec from_run_length =
  fun (encoded_list : (int * char) list) : char list ->
  let rec add_to_list times letter =
    match times with
    | 1 -> letter :: []
    | n -> letter :: add_to_list (n - 1) letter in
  match encoded_list with
  | [] -> []
  | [(num, ch)] -> add_to_list num ch
  | (num', ch') :: tl ->
     let rec merge = function
       | [], tail -> tail
       | (hd :: tl), tail -> hd :: merge(tl, tail) in
     merge( add_to_list num' ch', from_run_length tl )
;;

let to_run_length =
  fun (orig_list : char list) : (int * char) list ->
  (* Find the no of consecutive chars. The count and char pair is stored in tmp *)
  let rec count_char (lst : char list) (count : int ) (tmp : (int * char) list) =
    let rec appnd target item =
      match target with
      | [] -> [item]
      | hd :: tl -> hd :: appnd tl item in
    match lst with
    | [] -> []
    | [x] -> appnd tmp (count, x)
    | hd1 :: hd2 :: tl ->
       if hd1 = hd2 then count_char (hd2 :: tl) (count + 1) tmp
       else count_char (hd2 :: tl) 1 (appnd tmp (count, hd1)) in
  match orig_list with
  | [] -> []
  | [x] -> [(1, x)]
  | _ :: _ -> count_char orig_list 1 []
;;

(*======================================================================
Part 2 - Prisoner's Dilemma

In the remainder of this problem set, you will be implementing a version
of the iterated prisoner's dilemma in OCaml.

We represent an action as a boolean. The boolean value `true` represents 
a cooperation action, and the boolean value `false` represents a defection 
action, which we codify in some constant definitions. 

We represent a `play`, that is, one round of the prisoner's dilemma, as
a boolean tuple, where the first element represents your action and
the second element represents the other player's action. *)

type action = bool ;;

let cCOOPERATE = true ;;
let cDEFECT = false ;;

type play = action * action ;;

(* A payoff matrix will be represented as an association list, a list of
key-value pairs. the first entry of the tuple is a `play`, and the
second entry of the tuple is an `int * int` tuple, representing the
payoff to each player. The first element of the payoff is your payoff,
and the second element is the other player's payoff.

We will represent a history of actions using a play list. The head of
the list is the most recent round's actions. *)
                     
type payoff_matrix = (play * (int * int)) list

(* Note: Do *not* modify this matrix. *)

let test_payoff_matrix : payoff_matrix = 
  [ ( (cCOOPERATE, cCOOPERATE), (3, 3)  );
    ( (cCOOPERATE, cDEFECT),    (-2, 5) );
    ( (cDEFECT,    cCOOPERATE), (5, -2) );
    ( (cDEFECT,    cDEFECT),    (0, 0)  ) ] ;;


(*......................................................................
Problem 2a: Write a function `extract_entry` that given a `play` and a
`payoff_matrix` as input, returns the payoff for the given play. If
the play is not found in the payoff matrix, return a default tuple
value of (404, 404). (We'll introduce much better ways of signaling
error conditions later, as described in Chapter 10.)

Here is its signature:

    extract_entry : play -> payoff_matrix -> int * int

Replace the stub below with your own definition of `extract_entry`.
......................................................................*)

let rec extract_entry =
  fun (thisPlay : play) (game_payoff_matrix : payoff_matrix) : (int * int) ->
  match game_payoff_matrix with
  | [] -> (404, 404)
  | (act, payoff)::tl -> if act = thisPlay then payoff
    else extract_entry thisPlay tl
;;

(* We will represent a history of actions as a `play list`. The head
of the list is the most recent round's actions.

We also represent a strategy as a function from a history to an
action, that is, a value of type `history -> action`.

As an example, we define two basic strategies, `nasty` and `patsy`,
which will ignore their inputs and always defect or cooperate,
respectively. *)

type history = play list
type strategy = history -> action 

let nasty : strategy = 
  fun _ -> cDEFECT ;;

let patsy : strategy = 
  fun _ -> cCOOPERATE ;;

(*......................................................................
Problem 2b: The above strategies are not very sophisticated. Let us
start working our way up to defining a more complex strategy. To do so,
we will need to define two helper functions, `count_defections` and
`count_cooperations`. These functions will take as input a history, and
return a tuple containing the number of defections or cooperations that
you and the other player made, respectively.

Here are its signatures:

  count_defections : history -> int * int
  count_cooperations : history -> int * int

Replace the stubs below with your own definitions of `count_defections`
and `count_cooperations`. Try your best to reduce code duplication!
......................................................................*)

let count_defections = 
  fun (hist: history) : (int * int) ->
  let (myAction, herAction) = unzip hist in
  (List.fold_left(fun acc x -> if x = cDEFECT then acc + 1 else acc)0 myAction,
     List.fold_left(fun acc x -> if x = cDEFECT then acc + 1 else acc)0 herAction)
;;


let count_cooperations = 
  fun (hist: history) : (int * int) ->
  let (myAction, herAction) = unzip hist in
  (List.fold_left(fun acc x -> if x = cCOOPERATE then acc + 1 else acc)0 myAction,
     List.fold_left(fun acc x -> if x = cCOOPERATE then acc + 1 else acc)0 herAction)
;;
 

(*......................................................................
Problem 2c: Define a balanced strategy. This strategy cooperates on the
first round, and then does the opposite action of its previous round's
action for every subsequent round. Recall that a `strategy` is actually a
function of type `history -> action`.
......................................................................*)

let balanced =
  fun (hist: history) : action ->
  match hist with
  | [] -> cCOOPERATE
  | (myAction, _) :: _ -> if myAction = cCOOPERATE then cDEFECT
                                   else cCOOPERATE
;;
 

(*......................................................................
Problem 2d: Define an egalitarian strategy. This strategy cooperates
on the first round. On all subsequent rounds, the egalitarian strategy
examines the history of the other player's actions, counting the total
number of defections of each player. If the other player's defections
outnumber our defections, the strategy will defect; otherwise it will
cooperate.
......................................................................*)

let egalitarian =
  fun (hist: history) : action ->
  let (a, b) = count_defections hist in if b > a then cDEFECT
                                  else cCOOPERATE
;;
 

(*......................................................................
Problem 2e: Define a tit-for-tat strategy. This strategy cooperates on
the first round, and then on every subsequent round it mimics the other
player's previous action.
......................................................................*)

let tit_for_tat =
  fun (hist: history) : action ->
  match hist with
  | [] -> cCOOPERATE
  | (_, herAction) :: _ -> if herAction = cCOOPERATE then cCOOPERATE
                                   else cDEFECT
;; 

(*......................................................................
Problem 2f: Now define your own strategy. Any strategy that compiles
and is not the `failwith` stub will receive full credit. If you'd
like, you can run this strategy in a round-robin tournament as
described in the problem set writeup. You may assume that the
tournament will use the payoff matrix defined by the original
`test_payoff_matrix`.

For this problem only, you may make use of the `Random` module if you
would like to. See its documentation online at
<https://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html>.

If you want to enter your strategy in the tournament, give it a
pseudonym as well. To maintain grading anonymity, please don't use
your real name, and please use appropriate language in your pseudonym.
......................................................................*)

(* My strategy is to check whether a random number od 3 is equal to
   1. If so, I will adopt cCOOPEATE, else I will adopt cDEFECT
*)
let my_strategy =
    fun (hist: history) : action ->
  match hist with
  | [] -> cCOOPERATE
  | (_, herAction) :: _ -> if Random.int 1000 mod 3 = 1 then cCOOPERATE
                                   else cDEFECT
;;

(* If you want to enter your strategy in the tournament, give it a
pseudonym here. *)
let my_pseudonym = "" ;;

(* In order to run a full iterated prisoner's dilemma, we need a few
more auxiliary functions. You'll write these now. *)
 
(*......................................................................
Problem 2g: Write a function `swap_actions` that given a history
returns a history where each tuple is swapped, though the order of
rounds in the history should still be preserved. This function will be 
necessary for running two strategies against each other later in this
problem set. Here is its signature:

    swap_actions : history -> history

For example:

    swap_actions [(cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE)] 
               = [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)] 
......................................................................*)

let rec swap_actions =
  fun (hist: history) : history ->
  match hist with
  | [] -> []
  | (myAction, herAction) :: tl -> (herAction, myAction) :: swap_actions(tl);;
 
(*......................................................................
Problem 2h: Write a function `calculate_payoff` that given a
`payoff_matrix` and a history returns the total payoffs to you and
the other player generated by the given history. Here is its
signature:
       
    calculate_payoff : payoff_matrix -> history -> int * int 
......................................................................*)

let rec calculate_payoff =
  fun (match_payoff : payoff_matrix) (hist : history) : (int*int) ->
  (* sumPayOff is a helper function to add the payoffs of two games *)
  let sumPayOff (a, b) (c, d) = (a + c, b + d) in
  match hist with
  | [] -> (0, 0)
  | hd :: tl -> sumPayOff (extract_entry hd match_payoff)  (calculate_payoff match_payoff tl)
;;

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
