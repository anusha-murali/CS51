let deoptionalize (lst: 'a option list) : 'a list = 
List.map (fun x -> match x with Some a -> a)
   (List.filter (fun x -> x <> None) lst) ;;


let deoptionalize (lst: 'a option list) =
  let get_val (Some a) = a in
  List.map get_val (List.filter (fun x -> x <> None) lst)
;;

let deoptionalize (lst: 'a option list) =          
  let get_val  x = 
  match x with 
  | [] -> []
  | None -> []
  | Some v -> v
  in
  List.map get_val (List.filter (fun x -> x <> None) lst)
;;
