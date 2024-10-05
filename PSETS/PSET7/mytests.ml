Problem 1

let sample_end = ref Nil ;;
let cyclic = ref (Cons (1, ref (Cons (2, sample_end)))) ;;
sample_end := !cyclic ;;
let acyclic = ref (Cons (3, ref (Cons(4, ref Nil)))) ;;

let () = assert (has_cycle cyclic)
let () = assert (not(has_cycle acyclic))

let () = assert (mlength cyclic = 2)
let () = assert (mlength acyclic = 2)


Problem 7

let myTree = lazy(Node(5,[]));;

let myTree = lazy(Node(5, [lazy(Node(3, []))]));;

let myTree = lazy(Node(5, [lazy(Node(3, [])); lazy(Node(12, []))]));;
node myTree;;  (* should return 5 *)
node (List.hd(children myTree));;   (* should return 3 *)
node (List.hd (List.tl(children myTree)));; (* should return 12 *)

let myTree = lazy(Node(0, 
               [lazy(Node(1, [lazy(Node(3, [])); 
                              lazy(Node(4, []))])); 
                lazy(Node(2, [lazy(Node(5, []));
                              lazy(Node(6, []))]))]));;


(* Let us define a simple function *)
let myFunc x = x + 13;;
let newTree = tmap myFunc myTree;

(* Now check the value of the root node in newTree. It should be 13 *)
node newTree;;

(* Check the value of the second node. It should be 15 *)
node (List.hd (List.tl(children newTree)));;


(* Test for tmap2 *)
let myTree2 = lazy(Node(100,
               [lazy(Node(10, [lazy(Node(30, []));
                              lazy(Node(40, []))]));
                lazy(Node(20, [lazy(Node(50, []));
                              lazy(Node(60, []))]))]));;

let myFunc x y = x + y;;
let newTree = tmap2 myFunc myTree myTree2;;

(* Now check the value of the root node in newTree. It should be 100 *)
node newTree;;

(* Check the value of the second node. It should be 22 *)
node (List.hd (List.tl(children newTree)));;



