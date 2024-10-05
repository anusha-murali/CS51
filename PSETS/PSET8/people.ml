(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                      People in the simulation
 *)

module G = Graphics ;; 
open Config ;;
open Registry ;;
module Ctr = Counter ;;
module Viz = Visualization ;;
module Stat = Statistics ;; 
module Utilities = Utilities ;; 

(*....................................................................
                                People
 *)
  
class person (initx : int) (inity : int)
             (initstepsize : int)
             (initinfect : float) =
  object (self)
    val id : string = Utilities.gensym ()
    val mutable posx : int = initx
    val mutable posy : int = inity
    val mutable step_size : int = initstepsize
    val mutable infectiousness : float = initinfect
                  
    method id : string = id
    method step_size : int = step_size
    method infectiousness : float = infectiousness
                  
    method set_pos (x : int) (y : int) : unit =
      posx <- x;
      posy <- y
    method pos = posx, posy
                         
    method set_step_size (new_step_size : int) : unit =
      step_size <- new_step_size
                     
    method set_infectiousness (new_infect : float) : unit =
      infectiousness <- new_infect

    method move : unit =
      let x, y = self#pos in
      let newx, newy =
        Utilities.rand_step x y self#step_size in
      (* drop from old location in registry *)
      Reg.deregister (self :> thing_type);
      (* update location *)
      self#set_pos newx newy;
      (* re-add at the new location *)
      Reg.register (self :> thing_type)

    method update : unit =
      self#move
  
    method draw : unit =
      let x, y = self#pos in
      Viz.draw_circle x y G.black
  end ;;

(*....................................................................
                       People in various states

  Note that since these classes refer to each other, they must be
  simultaneously defined using `and` instead of sequentially defined
  as separate classes.
 *)
  
class susceptible (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_SUSCEPTIBLE
                   cINFECTIOUSNESS_SUSCEPTIBLE
            as super

    initializer
      Stat.susceptible#bump
                     
    method! update =
      super#update;
      let posx, posy = self#pos in
      (* calculate total infectiousness of all neighbors *)
      let infectiousness_total =
        Utilities.sum_float
          (List.map (fun obj -> obj#infectiousness)
                    (Reg.neighbors (self :> thing_type))) in
      (* if infected, update the registry by replacing this object
         with an infected one *)
      if Utilities.flip_coin infectiousness_total then
        begin
          Stat.susceptible#debump;
          Reg.deregister (self :> thing_type);
          Reg.register ((new infected posx posy) :> thing_type)
        end

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_SUSCEPTIBLE
  end

and (* class *) infected (initx : int) (inity : int) =
  object
    inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED

    initializer
      Stat.infected#bump

    (*.................................................................
      Place any augmentations to `infected` here.
    ................................................................ *)

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_INFECTED

    method! update =
      super#update;
      let posx, posy = self#pos in
        (* if recovery period is over *)
        if time_steps <= 0. then
          (* death with probability = cMORTALITY *)
          (if Utilities.flip_coin cMORTALITY then
            begin
              Stat.infected#debump;
              Registry.deregister (self :> thing_type);
              Registry.register ((new deceased posx posy) :> thing_type)
            end
          (* else update to recovery *)
          else
            begin
              Stat.infected#debump;
              Registry.deregister (self :> thing_type);
              Registry.register ((new recovered posx posy) :> thing_type)
            end)
        (* else decrement the time left to recovery *)
        else time_steps <- time_steps -. 1.

  end

and (* class *) recovered (initx : int) (inity : int) =
  object (self)
  inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED
            as super

    initializer
      Stat.recovered#bump

    val mutable immunity = Utilities.gaussian (fst cIMMUNITY_PERIOD) (snd cIMMUNITY_PERIOD)

    (* updated draw method w/ color *)
    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_RECOVERED

    (* update method *)
    method !update =
      super#update;
      let posx, posy = self#pos in
      (* check whether or not immunity is over *)
      if immunity <= 0. then
        begin
          Stat.recovered#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new susceptible posx posy) :> thing_type)
        end
      (* else decrement immunity time *)
      else immunity <- immunity -. 1.
  end

and (* class *) deceased (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED

    initializer
      Stat.deceased#bump

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_DECEASED
  end


(*....................................................................
Place definitions for any other classes here. In particular, you'll
want to at least implement a `recovered` class for `infected` people
who have recovered from the infection and a `deceased` class for
`infected` people who do not recover.
....................................................................*)
;;
