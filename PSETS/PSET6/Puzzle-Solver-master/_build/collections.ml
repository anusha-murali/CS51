(*
  This file provides the signature and several
  functors implementing specific collections (stacks, queues,
  etc.).
 *)

module type COLLECTION =
sig

  (* Empty -- Exception indicates attempt to take from an empty
     collection *)
  exception Empty

  (* elements in the collection *)
  type elt

  (* collections themselves *)
  type collection

  (* empty -- the empty collection, collection with no elements *)
  val empty : collection

  (* length col -- Returns number of elements in the collection col *)
  val length : collection -> int

  (* is_empty col -- Returns true if and only if the collection col is
     empty *)
  val is_empty : collection -> bool

  (* add elt col -- Returns a collection like col but with an element
     elt added *)
  val add : elt -> collection -> collection

  (* take col -- Returns a pair of an element from the collection and
     the collection of the remaining elements; raises Empty if the
     collection is empty. Which element is taken is determined by the
     implementation. *)
  val take : collection -> elt * collection

end

(*......................................................................
  Stacks implemented as lists
 *)

module MakeStackList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct
    exception Empty

    type elt = Element.t
    type collection = elt list

    let empty : collection = []

    let is_empty (d : collection) : bool =
      d = empty

    let length (d : collection) : int =
      List.length d

    let add (e : elt) (d : collection) : collection =
      e :: d;;

    let take (d : collection) :  elt * collection =
      match d with
      | hd :: tl -> (hd, tl)
      | _ -> raise Empty
end

(*......................................................................
  Queues implemented as lists
 *)

module MakeQueueList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct
    exception Empty

    type elt = Element.t
    type collection = elt list

    let empty : collection = []

    let length (d : collection) : int =
      List.length d

    let is_empty (d : collection) : bool =
      d = empty

    let add (e : elt) (d : collection) : collection =
      d @ [e];;

    let take (d : collection)  :  elt * collection =
      match d with
      | hd :: tl -> (hd, tl)
      | _ -> raise Empty
  end

(*......................................................................
  Queues implemented as two stacks *)

module MakeQueueStack (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct
    exception Empty
    module StackList = (MakeStackList(Element)
           : (COLLECTION with type elt = Element.t))

    type elt = Element.t
    type collection =
           {front : StackList.collection; revrear : StackList.collection}

    let empty : collection =
                  {front = StackList.empty; revrear = StackList.empty}

    let is_empty (d : collection) : bool =
      d = empty

    let length (d : collection) : int =
      StackList.length d.front + StackList.length d.revrear

    let add (e : elt) (d : collection) : collection =
      {front = d.front; revrear = StackList.add e d.revrear}

    let take (d : collection) : elt * collection =

      if is_empty d then raise Empty
      else if not (StackList.is_empty d.front) then
        match (StackList.take d.front) with
          | (e, col) -> (e, {front = col; revrear = d.revrear})

      else
        let rec switch (d : collection) : collection =
          if StackList.is_empty d.revrear then d
          else match StackList.take d.revrear with
               | (e, col) -> switch
                               {front = StackList.add e d.front; revrear = col}
          in
            let switched = switch d in
              match StackList.take (switched.front) with
                | (e, col) -> (e, {front = col; revrear = StackList.empty})

  end
