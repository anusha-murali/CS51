let t = IntTree.empty;;

let t = IntTree.insert 5 t;;

let t = IntTree.delete 5 t;;



(* Create a module called IntListQueue using the
   ListQueue functor implementation
*)
module IntListQueue = (ListQueue(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t);;

let q = IntListQueue.empty;;

IntListQueue.is_empty q;;

let q = IntListQueue.add 5 q;;


module IntTree = BinSTree(IntCompare);;


(* Create a module called IntTreeQueue using the
   TreeQueue functor implementation 
*)
module IntTreeQueue = (TreeQueue(IntCompare) :
                          PRIOQUEUE with type elt = IntCompare.t);;

let q = IntTreeQueue.empty;;



(* Create a module called IntHeapQueue using the 
   BinaryHeap functor implementation
*)
module IntHeapQueue = (BinaryHeap(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t);;

let q = IntHeapQueue.empty;;

let q = IntHeapQueue.add 5 q;;




