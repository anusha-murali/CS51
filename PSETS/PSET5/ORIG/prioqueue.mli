(*
                         CS 51 Problem Set 5
                Modules, Functors, and Priority Queues
                           Priority Queues
*)

open Order

module type PRIOQUEUE =
sig
  exception QueueEmpty

  type elt
  type queue

  val empty : queue
  val is_empty : queue -> bool
  val add : elt -> queue -> queue
  val take : queue -> elt * queue
  val to_string : queue -> string
  val run_tests : unit -> unit
end

module ListQueue(C : COMPARABLE) : (PRIOQUEUE with type elt = C.t)
module TreeQueue (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t)
module BinaryHeap (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t)

module IntHeapQueue : (PRIOQUEUE with type elt = IntCompare.t)
module IntTreeQueue : (PRIOQUEUE with type elt = IntCompare.t)
module IntListQueue : (PRIOQUEUE with type elt = IntCompare.t)
