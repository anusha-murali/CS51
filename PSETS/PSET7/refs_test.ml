(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                             Refs Testing
 *)

open CS51Utils ;;
open Absbook ;;

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = ref (Cons (2, ref Nil)) ;;
let list1b = ref (Cons (2, list1a)) ;;
let list1 = ref (Cons (1, list1b)) ;;

let reflist =  ref (Cons (2, ref Nil)) ;;
let list2 = ref (Cons (1, ref (Cons (2, reflist)))) ;;
let _ = reflist := !list2 ;;

let list3a = ref (Cons (3, ref (Cons (2, ref (Cons (3, ref (Cons (6, ref Nil))))))));;
let reflist3 = ref Nil;;
let list3b = ref (Cons (4, ref (Cons (3, ref (Cons (2, reflist3))))));;
let _ = reflist3 := !list3b;;
let list3c = ref (Cons (13, list3b));;
let list3d = ref (Cons (0, ref (Cons (2, ref (Cons (3, ref (Cons (12, ref Nil))))))));;

let reflist4 = ref Nil;;
let list4a= ref (Cons (1, ref (Cons (1, ref (Cons (1, ref (Cons (1, reflist4))))))));;
let _ = reflist4 := !list4a;;
let list4b= ref (Cons (3, ref (Cons (1, ref (Cons (1, ref (Cons (4, reflist4))))))));;

let reflist5 = ref Nil;;
let list5 = ref (Cons (0, reflist5));;
let _ = reflist5 := !list5;;
let list5b = ref (Cons (4, ref (Cons (3, ref (Cons (2, reflist5))))));;

let reflist6 = ref Nil;;
let list6a = ref (Cons (1, ref (Cons (2, ref (Cons (2, reflist6))))));;
let _ = reflist6 := !list6a;;
let list6b = ref (Cons (32, ref (Cons (678, ref (Cons (72, list6a))))));;
let list6c = ref (Cons (4, ref (Cons (3, ref (Cons (2, reflist6))))));;

let reflist7 = ref Nil;;
let list7a = ref (Cons (1, ref (Cons (1, ref (Cons (2, ref (Cons (3, ref (Cons (2, ref Nil))))))))));;
let list7b = ref (Cons (2, ref (Cons (3, ref (Cons (2, reflist7))))));;
let _ = reflist7 := !list7b;;
let list7c = ref (Cons (1, ref (Cons (1, list7b))));;

let list8a = ref (Cons (9, ref (Cons (0, ref (Cons (4, ref (Cons (2, ref (Cons (3, ref (Cons (2, ref Nil))))))))))));;
let reflist8 = ref Nil;;
let list8b = ref (Cons (2, ref (Cons (2, ref (Cons (3, ref (Cons (2, reflist8))))))));;
let _ = reflist8 := !list8b;;
let list8c = ref (Cons (1, ref (Cons (1, list8b))));;
let list8d = ref (Cons (2, ref (Cons (3, ref (Cons (2, reflist8))))));;

let reflist9 = ref Nil;;
let list9 = ref (Cons (0, reflist9));;
let _ = reflist8 := !list9;;
let list9b = ref (Cons (4, ref (Cons (3, ref (Cons (2, reflist9))))));;


let tests () =
  (* Tests for has_cycle *)

  unit_test (not (has_cycle list1a)) "list1a has no cycle";
  unit_test (has_cycle reflist)      "reflist has cycle";
  unit_test (not (has_cycle list3a)) "list3a has cycle";
  unit_test (has_cycle list3b)       "list3b has cycle";
  unit_test (has_cycle list4a)       "list4a has cycle";
  unit_test(has_cycle list5)         "list5 has cycle";
  unit_test(has_cycle list6a)        "list6a has cycle";
  unit_test (has_cycle list7b)       "list7b has cycle";
  unit_test (not (has_cycle list7a)) "list7a has no cycle";
  unit_test (not (has_cycle list8c)) "list8c has no cycle";
  unit_test (not (has_cycle list8b)) "list8b has no cycle";
  unit_test (not (has_cycle list8d)) "list8d has no cycle";
  unit_test (not (has_cycle list9)) "list9 has no cycle";

  (* Tests for flatten *)

  let _ = flatten list2 in
  assert (not (has_cycle list2));

  let _ = flatten list3c in
  assert (not(has_cycle list3c));

  let _ = flatten list4a in
  assert (not(has_cycle list4a));

  let _ = flatten list5 in
  assert (not(has_cycle list5));

  let _ = flatten list6b in
  assert (not(has_cycle list6b));

  let _ = flatten list7b in
  assert (not (has_cycle list7b)) ;

  let _ = flatten list8b in
  assert (not (has_cycle list8b)) ;

  (* Tests for mlength *)

  unit_test (mlength list1a = 1)   "mlength for list1a";
  unit_test (mlength list2 = 2)    "mlength for list2";
  unit_test (mlength list3a = 4)   "mlength for list3a";
  unit_test (mlength list3c = 2)   "mlength for list3c";
  unit_test (mlength list3b = 1)   "mlength for list3b";
  unit_test (mlength reflist = 0)  "mlength for reflist";
  unit_test ((mlength list5) = 1)  "mlength for list5";
  unit_test ((mlength list6b) = 2) "mlength for list6b";
  unit_test (mlength list7a = 5)   "mlength for list7a";
  unit_test (mlength list7c = 4)   "mlength for list7c";
  unit_test (mlength list8a = 6)   "mlength for list8a";
  unit_test (mlength list8b = 2)   "mlength for list8b";

  () ;;

let _ = tests () ;;
