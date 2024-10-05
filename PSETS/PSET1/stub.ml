let rec from_run_length =
  fun (encoded_list : (int * char) list) : char list ->
  let rec add_to_list times letter =
    match times with
    | 1 -> letter :: []
    | n -> letter :: add_to_list (n - 1) letter in
  match encoded_list with
  | [] -> []
  | [(num, ch)] -> add_to_list num ch
  | (num', ch') :: tl ->
     let rec merge = function
       | [], tail -> tail
       | (hd :: tl), tail -> hd :: merge(tl, tail) in
     merge( add_to_list num' ch', from_run_length tl )
;;

let to_run_length =
  fun (orig_list : char list) : (int * char) list ->
  (* Find the no of consecutive chars. The count and char pair is stored in tmp *)
  let rec count_char (lst : char list) (count : int ) (tmp : (int * char) list) =
    let rec appnd target item =
      match target with
      | [] -> [item]
      | hd :: tl -> hd :: appnd tl item in
    match lst with
    | [] -> []
    | [x] -> appnd tmp (count, x)
    | hd1 :: hd2 :: tl ->
       if hd1 = hd2 then count_char (hd2 :: tl) (count + 1) tmp
       else count_char (hd2 :: tl) 1 (appnd tmp (count, hd1)) in
  match orig_list with
  | [] -> []
  | [x] -> [(1, x)]
  | _ :: _ -> count_char orig_list 1 []
;;

