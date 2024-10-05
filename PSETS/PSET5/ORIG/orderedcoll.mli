(* 
                         CS 51 Problem Set 5
                Modules, Functors, and Priority Queues
             Ordered Collections and Binary Search Trees
*)

open Order
   
module type ORDERED_COLLECTION =
  sig
    exception Empty
    exception NotFound

    type elt
    type collection

    val empty : collection
    val search : elt -> collection -> bool
    val insert : elt -> collection -> collection
    val delete : elt -> collection -> collection
    val getmin : collection -> elt
    val getmax : collection -> elt
    val to_string : collection -> string
    val run_tests : unit -> unit
  end

module BinSTree (Elt : COMPARABLE)
              : (ORDERED_COLLECTION with type elt = Elt.t)

module IntTree : (ORDERED_COLLECTION with type elt = int)

val minutes_spent_on_pset : unit -> int
val reflection : unit -> string
