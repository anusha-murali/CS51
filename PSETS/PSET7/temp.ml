let rec interleave first period count l r () =
  if count = 0 then
    if first then
      interleave false period period r l ()
    else
      let period = 2 * period in
      interleave true period period r l ()
  else
    match l () with
    | Seq.Nil -> r ()
    | Cons(y,l) ->
      Cons(y, interleave first period (count-1) l r)

let interleave = interleave true 1 1

let rec to_seq (t: int tree list) () =
  let x = (node (List.hd t)) in
  let lst = (children (List.hd t)) in
  Seq.Cons(x, interleave (to_seq ([List.hd lst])) (to_seq (List.tl lst)))
