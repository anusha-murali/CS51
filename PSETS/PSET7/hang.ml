let list3a = ref (Cons (3, ref (Cons (2, ref (Cons (3, ref (Cons (6, ref Nil))))))));;
let reflist3 = ref Nil;;
let list3b = ref (Cons (4, ref (Cons (3, ref (Cons (2, reflist3))))));;
let _ = reflist3 := !list3b;;
let list3c = ref (Cons (13, list3b));;
let list3d = ref (Cons (0, ref (Cons (2, ref (Cons (3, ref (Cons (12, ref Nil))))))));;

(* let list3a = (Cons (3, ref (Cons (2, ref (Cons (3, ref (Cons (6, ref Nil))))))));;
let reflist3 = ref Nil;;
let list3b = (Cons (4, ref (Cons (3, ref (Cons (2, reflist3))))));;
let _ = reflist3 := !list3b;;
let list3c = Cons (13, list3b);;
let list3d = (Cons (0, ref (Cons (2, ref (Cons (3, ref (Cons (12, ref Nil))))))));; *)

