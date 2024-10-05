(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                      Section 2: Lazy Evaluation
                             Spring 2017
 *)
(*======================================================================
Section 2.1: Series acceleration with infinite streams

In nativeLazyStreams.ml, we provide the definitions of lazy streams
using OCaml's native Lazy module as presented in lecture, up to and
including code for approxiating pi through partial sums of the terms
in a Taylor series. In the next problem, you'll use streams to find
faster approximations for pi.

Recall from lecture the use of streams to generate approximations
of pi of whatever accuracy. Try it. You should be able to reproduce
the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within .001 of the value of pi.  This method converges quite
slowly. But we can increase the speed dramatically by averaging
adjacent elements in the approximation stream.
......................................................................*)

open NativeLazyStreams ;;

(*......................................................................
Problem 5: Implementing average on streams

Write a function average that takes a float stream and returns a
stream of floats each of which is the average of adjacent values in
the input stream. For example:

# first 5 (average (to_float nats)) ;;
- : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
......................................................................*)
  
let average (s : float stream) : float stream =
  let pairsums = smap2 ( +. ) s (tail s) in
  smap (( *. ) 0.5) pairsums
;;

(*......................................................................
Problem 6: Implementing Aitken's method

Now instead of using the stream of approximations in pi_sums, you can
instead use the stream of averaged pi_sums, which converges much more
quickly. Test that it requires far fewer steps to get within, say,
0.001 of pi.

An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set
writeup. Write a function to apply this accelerator to a stream, and
use it to generate approximations of pi.
......................................................................*)
   
let aitken (s: float stream) : float stream =
  let numerator = smap2 ( -. ) (tail (tail s)) (tail s) in
  let denominator_firstlastsum = smap2 ( +. ) (tail (tail s)) s in
  let denominator = smap2 ( -. ) denominator_firstlastsum 
                          (smap (( *. ) 2.0) (tail s)) in
  let numerator_sq = smap2 ( *. ) numerator numerator in
  let quotient = smap2 ( /. ) numerator_sq denominator in
  smap2 ( -. ) (tail (tail s)) quotient
;;

(*......................................................................
Problem 7: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi.

    ---------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  aitken method 
    ---------------------------------------------------------
    0.1      |     19    |         2         |      0
    ---------------------------------------------------------
    0.01     |    199    |         9         |      2
    ---------------------------------------------------------
    0.001    |    1999   |        30         |      6
    ---------------------------------------------------------
    0.0001   | UNCOMPUTED|         99        |      15
    ---------------------------------------------------------
    NOTE: our computers could not compute the number of steps required
    to get within 0.0001 of pi using pi_sums because the runtime took
    far too long.
......................................................................*)

(*======================================================================
Section 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type 'a
tree holds a value of some type 'a, and a (conventional, finite) list
of one or more (lazy) child trees. Complete the implementation by
writing print_depth, tmap, tmap2, and bfenumerate.  We recommend
implementing them in that order.
......................................................................*)
   
type 'a treeval = Node of 'a * 'a tree list
 and 'a tree = 'a treeval Lazy.t ;;

(* Infinite trees shouldn't have zero children. This exception is
available to raise in case that eventuality comes up. *)

exception Finite_tree ;;

(*......................................................................
Problem 8: Implementing infinite trees
*)

(*......................................................................
node t -- Returns the element of type 'a stored at the root node of
tree t of type 'a tree.
......................................................................*)
let node (t : 'a tree) : 'a =
  match Lazy.force t with
  | Node (x, _) -> x ;;

(*......................................................................
children t -- Returns the list of children of the root node of tree t.
......................................................................*)
let children (t : 'a tree) : 'a tree list =
  match Lazy.force t with
  | Node (_, child_list) -> child_list ;;

(*......................................................................
print_depth n indent t -- Prints a representation of the first n
levels of the tree t indented indent spaces. You can see some examples
of the intended output of print_depth below.
......................................................................*)
let rec print_depth (n : int) (indent : int) (t : int tree) : unit =
  if n < 0 then ()
  else 
    let rec printxspaces x =
      if x > 0 then
        let _ = Printf.printf " " in
        printxspaces (x - 1)
      else ()
    in
    let _ = printxspaces indent in
    let _ = Printf.printf "%d\n" (node t) in
    let _ = List.map (print_depth (n - 1) (indent + 1)) (children t) in ()

;;
(*......................................................................
tmap f t -- Returns a tree obtained by mapping the function f over
each node in t.
......................................................................*)
let rec tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match Lazy.force t with
  | Node (x, child_list) -> 
      lazy (Node ((f x), (List.map (tmap f) child_list))) ;;

(*......................................................................
tmap2 f t1 t2 -- Returns the tree obtained by applying the function f
to corresponding nodes in t1 and t2, which must have the same
"shape". If they don't an Invalid_argument exception is raised.
......................................................................*)
let rec tmap2 (f : 'a -> 'b -> 'c)
              (t1 : 'a tree) (t2 : 'b tree)
            : 'c tree =
  match (Lazy.force t1), (Lazy.force t2) with
  | Node (x1, child_list1), Node(x2, child_list2) ->
      lazy (Node ((f x1 x2), (List.map2 (tmap2 f) child_list1 child_list2)))

(*......................................................................
bfenumerate tslist -- Returns a LazyStreams.stream of the nodes in the
list of trees tslist enumerated in breadth first order, that is, the
root nodes of each of the trees, then the level one nodes, and so
forth. There is an example of bfenumerate being applied below.
......................................................................
 *)
let rec bfenumerate (tslist : 'a tree list) : 'a stream =
  match tslist with
  | h::t -> lazy (Cons (node h, bfenumerate (t @ (children h))))
  | [] -> raise Finite_tree
;;

(* Now use your implementation to generate some interesting infinite
trees.  Hint: Drawing a tree considering how the values change along
each branch will yield helpful intuition for the next problems. *)

(*......................................................................
onest -- An infinite binary tree all of whose nodes hold the integer 1.
......................................................................*)
let rec onest : int tree =
  lazy (Node (1, [onest; onest]));;

(*......................................................................
levels n -- Returns an infinite binary tree where the value of each
node in the tree is its level or depth in the tree, starting with the
argument n. For example:

# print_depth 2 0 (levels 0) ;;
0...
 1...
  2...
  2...
 1...
  2...
  2...
- : unit = ()
......................................................................*)
let rec levels (n : int) : int tree =
  lazy (Node (n, [(levels (n + 1)); (levels (n + 1))]));;

(*......................................................................
Define an infinite binary tree tree_nats where the value of each node in
the tree is consecutively numbered in breadth-first order starting
with 0. For example:

# print_depth 2 0 tree_nats ;;
0...
 1...
  3...
  4...
 2...
  5...
  6...
- : unit = ()
# first 10 (bfenumerate [tree_nats]) ;;
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
......................................................................*)
let tree_nats : int tree =
  let rec tree_nats_helper x = 
    lazy (Node(x, [(tree_nats_helper (2*x+1)); (tree_nats_helper (2*x+2))])) in
  tree_nats_helper 0
;;
                                                 
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 240 ;;
