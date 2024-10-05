(* Problem 2a *)

type action = bool ;;

let cCOOPERATE = true ;;

let cDEFECT = false ;;

type play = action * action ;;

type payoff_matrix = (play * (int * int)) list

let test_payoff_matrix : payoff_matrix = 
  [ ( (cCOOPERATE, cCOOPERATE), (3, 3)  );
    ( (cCOOPERATE, cDEFECT),    (-2, 5) );
    ( (cDEFECT,    cCOOPERATE), (5, -2) );
    ( (cDEFECT,    cDEFECT),    (0, 0)  ) ] ;;

let rec extract_entry (thisPlay : play) (game_payoff_matrix : payoff_matrix) : int * int =
 match game_payoff_matrix with
 | [] -> (404, 404)
 | (s, i)::tl -> if s = thisPlay then i
   else extract_entry thisPlay tl
;;

(* Problem 2b *)


type history = play list;;
type strategy = history -> action;; 

let nasty : strategy = 
  fun _ -> cDEFECT ;;

let patsy : strategy = 
  fun _ -> cCOOPERATE ;;


let size l = List.fold_left (fun acc _ -> acc + 1) 0 l;;


(* count_defection *)
let count_defections (hist: history) : (int * int) =
  let (myAction, herAction) = unzip hist in
  (List.fold_left(fun acc x -> if x = cDEFECT then acc + 1 else acc)0 myAction,
     List.fold_left(fun acc x -> if x = cDEFECT then acc + 1 else acc)0 herAction)
;;

(* count_cooperations *)
let count_cooperations (hist: history) : (int * int) =
  let (myAction, herAction) = unzip hist in
  (List.fold_left(fun acc x -> if x = cCOOPERATE then acc + 1 else acc)0 myAction,
     List.fold_left(fun acc x -> if x = cCOOPERATE then acc + 1 else acc)0 herAction)
;;


(* Problem 2c *)

let balanced (hist: history) : action =
  match hist with
  | [] -> cCOOPERATE
  | (myAction, herAction) :: tl -> if myAction = cCOOPERATE then cDEFECT
                                   else cCOOPERATE
;; 


(* Problem 2d *)

let egalitarian (hist: history) : action =
  let (a, b) = count_defections hist in if b > a then cDEFECT
                                  else cCOOPERATE
;;


(* Problem 2e *)

let tit_for_tat =
  fun (hist: history) : action ->
  match hist with
  | [] -> cCOOPERATE
  | (myAction, herAction) :: tl -> if herAction = cCOOPERATE then cDEFECT
                                   else cCOOPERATE
;;


(* Problem 2f *)

let my_strategy =
  fun _ -> failwith "my_strategy not implemented" ;;


(* Problem 2g *)

let rec swap_actions =
  fun (hist: history) : history ->
  match hist with
  | [] -> []
  | (myAction, herAction) :: tl -> (herAction, myAction) :: swap_actions(tl);;


