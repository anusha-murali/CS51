(*                                      
                         CS 51 Problem Set 6
                                Search
 
                             Collections
 *)

(*======================================================================
Before working on this problem set, read the problem set 6 writeup in
the file `readme.pdf`. It provides context and crucial information for
completing the problems. In addition, make sure that you are familiar
with the problem set procedures in the document "Problem set
procedures for CS51".

You are allowed (and encouraged) to work with a partner on this
problem set. You are also allowed to work alone, if you prefer. See
https://cs51.io/procedures/pset-instructions/#working-with-a-partner
for further information on working with partners on problem sets.
======================================================================*)

(* The `COLLECTION` module signature is a generic data structure
   generalizing stacks, queues, and priority queues, allowing adding
   and taking elements. This file provides the signature and several
   functors implementing specific collections (stacks, queues,
   etc.).
 *)
module type COLLECTION = 
  sig

    (* Empty -- Exception indicates attempt to take from an empty
       collection *)
    exception Empty
                
    (* elements in the collection *)
    type elt
           
    (* collections themselves *)
    type collection
           
    (* empty -- the empty collection, collection with no elements *)
    val empty : collection 
                  
    (* length col -- Returns number of elements in the collection col *)
    val length : collection -> int
                                
    (* is_empty col -- Returns true if and only if the collection col is
       empty *)
    val is_empty : collection -> bool
                                  
    (* add elt col -- Returns a collection like col but with an element
       elt added *)
    val add : elt -> collection -> collection
                                    
    (* take col -- Returns a pair of an element from the collection and
       the collection of the remaining elements; raises Empty if the
       collection is empty. Which element is taken is determined by the
       implementation. *)
    val take : collection -> elt * collection
                                    
  end
  
(*----------------------------------------------------------------------
  Some useful collections

  To think about: For each of these implementations, what is the time
  complexity for adding and taking elements in this kind of
  collection?  *)

(*......................................................................
  Stacks implemented as lists 
 *)
    
module MakeStackList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) = 
  struct
    exception Empty

    type elt = Element.t
    type collection = elt list

    let empty : collection = []

    let is_empty (d : collection) : bool = 
      d = empty 
            
    let length (d : collection) : int = 
      List.length d
                  
    let add (e : elt) (d : collection) : collection = 
      e :: d ;;
      
    let take (d : collection) :  elt * collection = 
      match d with 
      | hd :: tl -> (hd, tl)
      | _ -> raise Empty
end

(*......................................................................
  Queues implemented as lists 
 *)
  
module MakeQueueList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct        
    exception Empty
                
    type elt = Element.t
    type collection = elt list
                          
    let empty : collection = []
                               
    let length (d : collection) : int = 
      List.length d
                  
    let is_empty (d : collection) : bool = 
      d = empty 
            
    let add (e : elt) (d : collection) : collection = 
      d @ [e] ;;
      
    let take (d : collection)  :  elt * collection = 
      match d with 
      | hd :: tl -> (hd, tl)
      | _ -> raise Empty
  end

(*......................................................................
  Queues implemented as two stacks
  
  In this implementation, the queue is implemented as a pair of stacks
  (s1, s2) where the elements in the queue from highest to lowest
  priority (first to last to be taken) are given by s1 @ s2R (where @
  is the appending function and s2R is the reversal of s2). Elements
  are added (in stack regime) to s2, and taken from s1. When s1 is
  empty, s2 is reversed onto s1. See Section 15.5.2 in Chapter 15 for
  more information on this technique. *)

module MakeQueueStack (Element : sig type t end) 
       : (COLLECTION with type elt = Element.t) =
  struct
    exception Empty

    type elt = Element.t

    (* The stacks can be implemented as lists and the two stacks S1 
       and S2 can be packaged together in a record *)
    type collection = {s1 : elt list; s2: elt list}

    let empty : collection = {s1=[]; s2=[]}

    let length (d : collection) : int =
      (List.length d.s1 + List.length d.s2)

    let is_empty (d : collection) : bool =
      (d.s1 = []) && (d.s2 = [])

    let add (e : elt) (d : collection) : collection =
      {s1 = d.s1; s2 = e::d.s2} ;;

    let rec take (d : collection)  :  elt * collection =
      match d.s1 with
      | h :: t -> (h, {s1 = t; s2 = d.s2})
      | [] -> if (d.s2 = []) then raise Empty
              else (take {s1 = (List.rev d.s2); s2 = []})
  end

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete. (If you worked with a partner, we're asking for how much time
each of you (on average) spent on the problem set, not in total.)
......................................................................*)

let minutes_spent_on_pset () : int =
  600 ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set. Where
did you run into problems and how did you end up resolving them? What
might you have done in retrospect that would have allowed you to
generate as good a submission in less time? Please provide us your
thoughts on these questions and any other reflections in the string
below.
......................................................................*)

let reflection () : string =
  "This was a difficult PSET due to the use of higher order functors" ;;
