  module MakePuzzleSolver
         (MakeCollection
            : functor (Element : sig type t end) ->
                      (COLLECTION with type elt = Element.t))
         (G : PUZZLEDESCRIPTION)
         (Element : sig type t end)
       : (PUZZLESOLVER with type state = G.state
                        and type move = G.move) =
  struct
    exception CantReachGoal

(*    module Collection = MakeCollection(Element) *)

    type state = G.state

    type move = G.move

    module Pending = MakeCollection(
      struct
        type t = state * (move list)
      end)

    module Visited = Set.Make(
      struct
        type t = state
        let compare = G.compare_states
      end)

    let solve () : (move list * state list) =
      let pending = Pending.add (G.initial_state, []) Pending.empty in
      let visited = Visited.singleton G.initial_state in
      let expanded = [] in

      let rec search (p : Pending.collection) (v : Visited.t) (e : state list) =
        if Pending.is_empty p then raise CantReachGoal
        else
          let (current_state, movelist), col = Pending.take p in
            if G.is_goal current_state then List.rev movelist, List.rev e
            else
              let rec add_n (n : (state * move) list)
                            (p2 : Pending.collection)
                            (v2 : Visited.t)
                            (moves : move list) =
              match n with
              | [] -> p2, v2
              | (state, mv) :: tl ->
                  if not (Visited.mem state v2)
                    then add_n tl (Pending.add (state, mv :: moves) p2)
                                  (Visited.add state v2)
                                  moves
                  else add_n tl p2 v2 moves in
                  let p3, v3 = add_n (G.neighbors current_state) col v movelist in
              search p3 v3 (current_state :: e) in
        search pending visited expanded

    let print_state = G.print_state

  end ;;

