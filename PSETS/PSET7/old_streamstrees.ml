(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                       Part 2: Lazy Evaluation
 *)

(*======================================================================
Section 2.1: Series acceleration with infinite streams

In the file `nativeLazyStreams.ml`, we provide the definitions of
lazy streams using OCaml's native `Lazy` module as presented in
Section 17.3 in the textbook, and in the file `sampleStreams.ml`, we
provide some sample streams from that chapter, up to and including
code for approximating pi through partial sums of the terms in a
Taylor series (Section 17.4). In lab 15, you used streams to find
faster approximations for pi by averaging adjacent elements in the
stream. In this section, you'll use Aitken's method to generate even
faster convergence to pi. *)
   
open NativeLazyStreams ;;

(* Recall from Section 17.4 the use of streams to generate
approximations of pi of whatever accuracy. Try it. You should be able
to reproduce the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within 0.001 of the value of pi. This method converges quite
slowly. 

But we can increase the speed dramatically using a technique called
"series acceleration". In lab 15, you began experimenting with series
acceleration, speeding up the convergence of the `pi_sums` stream by
averaging its elements. The `average` function takes a `float stream`
and returns a stream of `float`s each of which is the average of
adjacent values in the input stream. For example:

    # first 5 (average (to_float nats)) ;;
    - : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
 *)

let average (s : float stream) : float stream =
  smap2 (fun x y -> (x +. y) /. 2.0) s (tail s) ;;

(* Now instead of using the stream of approximations in `pi_sums`, we
can instead use the stream of averaged `pi_sums`, which converges much
more quickly:

    # within 0.01 (average pi_sums) ;;
    - : int * float = (9, 3.13707771416749859)
    # within 0.001 (average pi_sums) ;;     
    - : int * float = (30, 3.14209630544471885)
    # within 0.0001 (average pi_sums) ;;
    - : int * float = (99, 3.14154315231477277)
    # within 0.00001 (average pi_sums) ;;
    - : int * float = (315, 3.14158766221239905)
    # within 0.000001 (average pi_sums) ;;
    - : int * float = (999, 3.14159215408966919)
    # within 0.0000001 (average pi_sums) ;;
    - : int * float = (3161, 3.1415926035968269)

We've placed a table below of the number of steps required for the
`pi_sums` and `average pi_sums` methods.

An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set writeup in
Section 17.8.3. *)
  
(*......................................................................
Problem 4: Implementing Aitken's method

Write a function `aitken` to apply this accelerator to a stream, and
use it to generate approximations of pi.
Example use: within 0.0001 (aitken pi_sums)
......................................................................*)

let rec aitken (s: float stream) : float stream =
  let s_n = head s in  
  let s_n1 = head (tail s) in
  let s_n2 = head (tail (tail s)) in
  let a = s_n  -. ((s_n -. s_n1) ** 2.) /.  (s_n -. 2. *. s_n1 +. s_n2) in
  lazy (Cons (a, aitken (tail s)));;
;;

(*......................................................................
Problem 5: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi using Aitken's method.
 *)
(*
    -------------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  Aitken's method
    -------------------------------------------------------------
    0.1      |        19 |                 2 |                0
    -------------------------------------------------------------
    0.01     |       199 |                 9 |                2
    -------------------------------------------------------------
    0.001    |      1999 |                30 |                6
    -------------------------------------------------------------
    0.0001   |     19999 |                99 |               15
    -------------------------------------------------------------
    0.00001  |    199999 |               315 |               35
    -------------------------------------------------------------
    0.000001 |  too many |               999 |               77
    -------------------------------------------------------------
 *)
(*....................................................................*)

(*======================================================================
Section 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type `'a
tree` holds a value of some type `'a`, and a (conventional, finite)
list of one or more (lazy) child trees. Complete the implementation by
writing `print_depth`, `tmap`, `tmap2`, and `bfenumerate`. We
recommend implementing them in that order.
......................................................................*)
   
type 'a tree_internal = Node of 'a * 'a tree list
 and 'a tree = 'a tree_internal Lazy.t ;;

(* Infinite trees shouldn't have zero children. This exception is
available to raise in case that eventuality comes up. *)

exception Finite_tree ;;

(*......................................................................
Problem 6: Implement the following functions for manipulating
infinite trees. *)

(*......................................................................
node t -- Returns the element of type `'a` stored at the root node of
tree `t` of type `'a tree`.
......................................................................*)
  
let node (t : 'a tree) : 'a =
  match t with
  | lazy(Node(h,_)) -> h
  | exception (Finite_tree) -> raise Finite_tree
;;

(*......................................................................
children t -- Returns the list of children of the root node of tree `t`.
......................................................................*)
   
let children (t : 'a tree) : 'a tree list =
  match t with
  | lazy(Node(_,lst)) -> lst
  | exception (Finite_tree) -> raise Finite_tree
;;

(*......................................................................
print_depth n indent t -- Prints a representation of the first `n`
levels of the tree `t` indented `indent` spaces. You can see some
examples of the intended output of `print_depth` below.
......................................................................*)
   
let rec print_depth (n : int) (indent : int) (t : int tree) : unit =
  let rec indent_str s m =
   if m = 0 then "" 
   else s ^ indent_str s (m-1)
  in
  if (n = 0) then 
      Printf.printf "%s %d\n" (indent_str " " (10-n*(indent+1))) ((node t) - 1)
  else 
    let print_list x =
      print_depth (n-1) indent x;
    in
    Printf.printf "%s %d\n" (indent_str " " (10-n*(indent+1))) ((node t) - 1);
    List.iter print_list (children t);
;;




(*......................................................................
tmap f t -- Returns a tree obtained by mapping the function `f` over
each node in `t`.
......................................................................*)
   
let rec tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | lazy(Node(hd, tl)) -> lazy(Node((f hd, (List.map (fun x -> tmap f x) tl))))
  | exception (Finite_tree) -> raise Finite_tree
;;
  
(*......................................................................
tmap2 f t1 t2 -- Returns the tree obtained by applying the function
`f` to corresponding nodes in `t1` and `t2`, which must have the same
"shape". If they don't, an `Invalid_argument` exception is raised.
......................................................................*)
   
let rec tmap2 (f : 'a -> 'b -> 'c)
          (t1 : 'a tree) (t2 : 'b tree)
        : 'c tree =
  match t1, t2 with
  | lazy(Node(hd1, tl1)), lazy(Node(hd2, tl2)) ->
       lazy(Node((f hd1 hd2, (List.map2 (fun x y -> tmap2 f x y) tl1 tl2))))
  | exception (Finite_tree) -> raise Finite_tree
;;

(*......................................................................
bfenumerate tree_list -- Returns a `stream` of the nodes in the list
of trees `tree_list` enumerated in breadth-first order, that is, the
root nodes of each of the trees, then the level one nodes, and so
forth. There is an example of `bfenumerate` being applied below. If
there isn't an infinite set of nodes in the list of trees (think about
how that could come about), raise a `Finite_tree` exception.
......................................................................*)
   
let rec bfenumerate (tree_list : 'a tree list) : 'a stream =
  match tree_list with
  | [] -> raise Finite_tree 
  | hd::tl -> Cons (node hd, bfenumerate (tl));  bfenumerate (children hd)
;;

let rec bfenumerate (tree_list : 'a tree list) : 'a stream =
  lazy (Cons (node (List.hd tree_list), bfenumerate (List.tl tree_list)))
;;


let rec bfenumerate (tree_list : 'a tree list) : 'a stream =
  if (List.tl tree_list) <> [] then
    lazy (Cons (node (List.hd tree_list), (bfenumerate (List.tl tree_list))))
  else
    (bfenumerate (children (List.hd tree_list)))
;;

let rec bfenumerate (tree_list : 'a tree list) : 'a stream =
  if (List.tl tree_list) <> [] then (
    lazy (Cons (lazy (Cons (node (List.hd tree_list), (bfenumerate (List.tl tree_list)))),
                bfenumerate (children (List.hd tree_list))))
  )
  else
    lazy (Cons (node (List.hd tree_list), (bfenumerate (children (List.hd tree_list)))))
;;

(* GOOD ONE *)
let rec bfenumerate (tree_list : 'a tree list) : 'a stream =
  if (List.tl tree_list) <> [] then
    lazy (Cons (node (List.hd tree_list), (bfenumerate (List.tl tree_list))))
  else
    lazy (Cons (node (List.hd tree_list), (bfenumerate (children (List.hd tree_list)))))
;;

let rec merge (s1:int stream) (s2:int stream) : int stream =
  match s1, s2 with
  | lazy (Cons(hd1, tl1)), lazy (Cons(hd2, tl2)) ->
    if (hd1 < hd2) then lazy (Cons(hd1, (merge (tl1) s2)))
    else if (hd1 > hd2) then lazy (Cons(hd2, (merge s1 (tl2))))
    else lazy(Cons(hd1, (merge (tl1) (tl2))))
;;

(* WORKING *)
let rec bfenumerate (tree_list : 'a tree list) : 'a stream =
  if (List.tl tree_list) <> [] then
    merge (lazy (Cons (node (List.hd tree_list), (bfenumerate (List.tl tree_list)))))
      (bfenumerate (children (List.hd tree_list)))
  else
    lazy (Cons (node (List.hd tree_list), (bfenumerate (children (List.hd tree_list)))))
;;



let rec bfenumerate (tree_list : 'a tree list) : 'a stream =
  if (List.tl tree_list) <> [] then (
    lazy (Cons (
                lazy (Cons (node (List.hd tree_list), (bfenumerate (List.tl tree_list)))),
       bfenumerate (children (List.hd tree_list)))
    )
  )
  else
    lazy (Cons (node (List.hd tree_list), (bfenumerate (children (List.hd tree_list)))))
;;



(* Now you'll use your implementation to generate some interesting
infinite trees.  Hint: Drawing a tree considering how the values
change along each branch will yield helpful intuition for the next
problems. *)

(*......................................................................
onest -- An infinite binary tree all of whose nodes hold the integer 1.
......................................................................*)
   
let rec onest : int tree =
  lazy (Node(1, [onest; onest])) ;;

(*......................................................................
levels n -- Returns an infinite binary tree where the value of each
node in the tree is its level or depth in the tree, starting with the
argument `n`. For example:

    # print_depth 2 0 (levels 0) ;;
    0
     1
      2...
      2...
     1
      2...
      2...
    - : unit = ()
......................................................................*)
   
let levels (n : int) : int tree =
  let rec stream m =
    lazy (Node(m+1, [stream (m+1); stream (m+1)])) in
  stream n 
;;

let rec print_depth2 (n : int) (t : int tree) : unit =
  if (n = 0) then
      Printf.printf "%d\n" ((node t) - 1)
  else
    let print_list x =
      Printf.printf "%d\n" ((node x) - 1); 
    in
    List.iter print_list (children t);
    let print_list2 x =
      print_depth2 (n-1) x
    in
    List.iter print_list2 (children t);
;;

(*......................................................................
tree_nats -- An infinite binary tree where the value of each
node in the tree is consecutively numbered in breadth-first order
starting with 0. For example:

    # print_depth 2 0 tree_nats ;;
    0
     1
      3...
      4...
     2
      5...
      6...
    - : unit = ()

    # first 10 (bfenumerate [tree_nats]) ;;
    - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
......................................................................*)
   
let tree_nats : int tree =
  let rec stream m =
    lazy (Node(m, [stream (2*m); stream (2*m + 1)])) in
  stream 1
;;

(*======================================================================
Reflection on the problem set

     Please fill out the information about time spent and your
     reflection thereon in the file `refs.ml`.
 *)
