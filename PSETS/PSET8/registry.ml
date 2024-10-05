(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                  A registry of things in the world

Allows for storing a set of objects allowing for

-- registering and deregistering objects;
-- obtaining a list of all currently registered objects; and
-- obtaining a list of all currently registered objects within a
   certain distance from a given object.

Stores the registered objects in two data structures, a `Set` for easy
access to the full set of objects and a 2D array by location allowing
for more efficient access to the neighboring objects. *)

open Array ;;
(* open CS51Utils ;; *)

open Config ;;
  
(*....................................................................
  The objects in the world
 *)
 
class type thing_type =
  object
    method id : string
    method pos : int * int
    method infectiousness : float
    method step_size : int

    method set_pos : int -> int -> unit
    method set_infectiousness : float -> unit
    method set_step_size : int -> unit

    method update : unit
    method draw : unit
  end ;;

(*....................................................................
  A thing_type registry
 *)

module type REGISTRY =
  sig
    (* register obj -- Adds the `obj` to both the registry and the
       map. *)
    val register : thing_type -> unit
      
    (* deregister obj -- Removes the `obj` from both data
       structures. Raises `Not_found` if the `obj` was not previously
       registered. *)
    val deregister : thing_type -> unit

    (* registrants () -- Returns a list of all of the current
       registrants. *)
    val registrants : unit -> thing_type list
      
    (* neighbors obj -- Returns a list of all of the registrants that are
       within a fixed radius (by L2 distance) from the `obj`. *)
    val neighbors : thing_type -> thing_type list
  end

module Reg : REGISTRY =
  struct
    (* registrants -- An updatable set of all of the registered objects *)
    module Registrants =
      Set.Make (struct type t = thing_type
                       let compare = compare
                end) ;;

    let registrants = ref Registrants.empty ;;

    (* map -- A "map" of all the registered objects, organized by
       2-D location *)
    let map : thing_type list array array =
      make_matrix cX_DIMENSION cY_DIMENSION [] ;;

    (* The required REGISTRY functions. See REGISTRY module signature
       for documentation *)
      
    let register (obj : thing_type) : unit =
      registrants := Registrants.add obj !registrants;
      let x, y = obj#pos in
      map.(x).(y) <- obj :: map.(x).(y) ;;
      
    let deregister (obj : thing_type) : unit =
      let new_registrants = Registrants.remove obj !registrants in
      if new_registrants == !registrants then
        (* no obj removed; as of v4.03, physical equality guaranteed *)
        raise Not_found
      else
        begin
          registrants := new_registrants;
          let x, y = obj#pos in
          map.(x).(y) <- List.filter (fun old -> old#id <> obj#id)
                                       map.(x).(y)
        end ;;
      
    let registrants () = Registrants.elements !registrants ;;
  
    let neighbors (obj : thing_type) : thing_type list =
      let dist_squared = cNEIGHBOR_RADIUS * cNEIGHBOR_RADIUS in
      let x, y = obj#pos in
      
      (* all coordinates in a block around `obj` *)
      let open Utilities in
      cross_product (Absbook.range (x_bounded (x - cNEIGHBOR_RADIUS))
                                   (x_bounded (x + cNEIGHBOR_RADIUS)))
                    (Absbook.range (y_bounded (y - cNEIGHBOR_RADIUS))
                                   (y_bounded (y + cNEIGHBOR_RADIUS)))
      (* extract object lists at those coordinates if within the
         radius *)
      |> List.map (fun (nx, ny) ->
                   if (nx - x) * (nx - x) + (ny - y) * (ny - y)
                      <= dist_squared
                   then
                     map.(nx).(ny)
                   else [])
      (* and concatenate them *)
      |> List.concat ;;

  end
