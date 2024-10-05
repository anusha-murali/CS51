let in_order tree =
  let rec aux tree accumulator =
    match tree with
    | [] -> accumulator
    | hd::tl -> 
      let acc_with_right = aux tl accumulator in
      let acc_with_value = (node hd) :: acc_with_right in
      aux left acc_with_value in
  aux tree [] 


