(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                             Refs Testing
                             Spring 2017
 *)

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = Cons (2, ref Nil) ;;
let list1b = Cons (2, ref list1a) ;;
let list1 = Cons (1, ref list1b) ;;

let reflist = ref (Cons (2, ref Nil)) ;;
let list2 = Cons (1, ref (Cons (2, reflist))) ;;
let _ = reflist := list2 ;;

let list3a =Cons (1, ref (Cons (2, ref (Cons (3, ref (Cons (2, ref Nil)))))));;
let reflist3 = ref Nil;;
let list3b = Cons (2, ref (Cons (3, ref (Cons (2, reflist3)))));;
let _ = reflist3 := list3b;;
let list3c = Cons (1, ref list3b);;

let reflist4 = ref Nil;;
let list4a=Cons (1, ref (Cons (1, ref (Cons (1, ref (Cons (1, reflist4)))))));;
let _ = reflist4 := list4a;;

let reflist5 = ref Nil;;
let list5 = Cons (1, reflist5);;
let _ = reflist5 := list5;;

let reflist6 = ref Nil;;
let list6a = Cons (1, ref (Cons (2, ref (Cons (2, reflist6)))));;
let _ = reflist6 := list6a;;
let list6b = Cons (32, ref (Cons (678, ref (Cons (72, ref list6a)))));;

let list7a = Cons (1, ref (Cons (1, ref (Cons (2, ref (Cons (3, ref (Cons (2, ref Nil)))))))));;
let reflist7 = ref Nil;;
let list7b = Cons (2, ref (Cons (3, ref (Cons (2, reflist7)))));;
let _ = reflist7 := list7b;;
let list7c = Cons (1, ref (Cons (1, ref list7b)));;

let list8a = Cons (1, ref (Cons (1, ref (Cons (2, ref (Cons (2, ref (Cons (3, ref (Cons (2, ref Nil)))))))))));;
let reflist8 = ref Nil;;
let list8b = Cons (2, ref (Cons (2, ref (Cons (3, ref (Cons (2, reflist8)))))));;
let _ = reflist8 := list8b;;
let list8c = Cons (1, ref (Cons (1, ref list8b)));;


let test_has_cycle () =
  assert (not (has_cycle list1a)) ;
  assert (has_cycle !reflist) ;
  assert (not (has_cycle list3a)) ;
  assert (has_cycle list3b) ;
  assert (has_cycle list3c) ;
  assert (has_cycle list4a) ;
  assert(has_cycle list5);
  assert(has_cycle list6a);
  assert(has_cycle list6b);
  assert (has_cycle list7b) ;
  assert (not (has_cycle list7a)) ;
  assert (has_cycle list8c) ;
  assert (has_cycle list8b) ;

  ()
;;

let test_flatten () = 

  let _ = flatten list2 in
  assert (not (has_cycle list2));
  assert (list2 = Cons (1, ref (Cons (2, ref Nil))));

  let _ = flatten list3c in
  assert (not(has_cycle list3c));
  assert(list3c = list3a);

  let _ = flatten list4a in
  assert (not(has_cycle list4a));
  assert (list4a = Cons(1, ref (Cons(1, ref(Cons (1, 
                                                  ref(Cons(1,ref Nil))))))));

  let _ = flatten list5 in
  assert (not(has_cycle list5));
  assert (list5 = Cons (1, ref Nil));
  
  let _ = flatten list6b in
  assert (not(has_cycle list6b));
  assert (list6b = Cons (32, ref (Cons (678, ref (Cons (72, ref (Cons (1, 
    ref (Cons (2, ref(Cons (2, ref Nil))))))))))));

  let _ = flatten list7b in
  assert (not (has_cycle list7b)) ;
  assert (list7c = list7a) ;

  let _ = flatten list8b in
  assert (not (has_cycle list8b)) ;
  assert (list8c = list8a) ;
  ()
;;

let test_mlength () = 

  assert (mlength list1a = 1) ;
  assert (mlength list2 = 2) ;
  assert (mlength list3a = 4) ;
  assert (mlength list3c = 4) ; 
  assert (mlength list3b = 3) ;
  assert (mlength !reflist = 2) ;
  assert ((mlength Nil) = 0) ;(* what is the length of a Nil list??? *)
  assert ((mlength list5) = 1);
  assert ((mlength list6b) = 6); 
  assert (mlength list7a = 5) ;
  assert (mlength list7c = 5) ;
  assert (mlength list8a = 6) ;
  assert (mlength list8b = 4) ;
()
;;


test_has_cycle ();;
test_mlength();;
test_flatten ();;
