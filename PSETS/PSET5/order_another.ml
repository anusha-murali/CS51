(*
                          CS51 Problem Set 5
                Modules, Functors, and Priority Queues
             Ordered Collections and Binary Search Trees

                               TESTING
*)

open Order

(* Contains more COMPARABLE modules for autograding testing purposes; note that
StringLengthCompare's generate_lt doesn't work very well repeatedly for
generating trees to test -- after a while the only string you can shorten to
 is "", and we wouldn't be able to expect all of these ""'s to be stored
 higher than each other in a BST created using StringLengthCompare. *)

(* Yet another example implementation of the COMPARABLE module type for
strings. It uses string length in comparisons as opposed to alphabetical order.
This is to catch students' mistaken use of OCaml's built in comparison or < > =
functions -- and we're using string length precisely to circumvent them
(they are incredibly polymorphic). *)
       
module StringLengthCompare : (COMPARABLE with type t = string) =
  struct
    type t = string
    let compare s1 s2 =
      let l1, l2 = String.length s1, String.length s2 in
        if l1 < l2 then Less else if l1 > l2 then Greater else Equal

    let to_string s = s

    let () = Random.self_init ()

    let generate () = string_of_int 
                        (Random.int (int_of_float (2. ** 30. -. 2.)))

    let generate_gt s = s ^ " ++"

    let generate_lt s =
      try
        String.sub s 0 ((String.length s) - 1)
      with
      (* In the case that there's nothing to shorten from *)
      | _ -> ""

    let generate_between s1 s2 =
      let l1, l2 = String.length s1, String.length s2 in
        let lower, higher = min l1 l2, max l1 l2 in
        if higher - lower < 2 then None else Some (generate_lt s2)
  end
