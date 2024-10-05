ocamlc collections.ml puzzledescription.ml puzzlesolve.ml
rlwrap ocaml unix.cma collections.cmo puzzledescription.cmo puzzlesolve.cmo


module StackModule = (MakeStackList (struct type t=int end) : COLLECTION with type elt = int);;

let s = StackModule.empty;;
let s = StackModule.add 5 s;;
let s = StackModule.add 12 s;;
StackModule.is_empty s;;
let (a, b) = StackModule.take s;;   (* a = 12 and b = [5] *)

module QueueModule = (MakeQueueList (struct type t=int end) : COLLECTION with type elt = int);;
let q = QueueModule.empty;;
let q = QueueModule.add 5 q;;
let q = QueueModule.add 12 q;;
QueueModule.is_empty q;;
let (a, b) = QueueModule.take q;;   (* a = 5 and b = [12] *)


module QueueStackModule = (MakeQueueStack (struct type t=int end) : COLLECTION with type elt = int);;
let q = QueueStackModule.empty;;
let q = QueueStackModule.add 5 q;;
let q = QueueStackModule.add 12 q;;
QueueStackModule.is_empty q;;
let (a, b) = QueueStackModule.take q;;   (* a = 5 and b = [12] *)


let open MakeQueueStack in
let q = empty in
add 1 q;
add 2 q;
take q ;;

