let has_cycle (lst : 'a mlist) : bool =

  (* helper that checks list up to e for whether e points to list *)
  let rec checksofar lst' e : bool =
    match lst' with
    |Nil -> false
    |Cons (x, r) -> (match e with
		     |Nil -> false (* should never happen, but putting an
				    * assert stmt here before false gave
				    * warning (why?) / but compiled *)
		     |Cons (x', r') -> 
		       (match !r' with (* if e's ref -> lst then cyclic *)
			|Nil -> false (* if e points to nil quit early *)
			|Cons (x'', r'') ->
			  if (x = x'') && ((!r) == (!r'')) then true
			  else (*check if e = lst before recurring *)
			    (* note that MUST do it in this order to catch
			     * elements that refer to themselves *)
			    if (x = x') && ((!r) == (!r')) then
			      checksofar lst (!r')
			    else checksofar (!r) e))
  in

  checksofar lst lst
