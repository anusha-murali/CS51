(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                     Parameters of the simulation
 *)
                 
(*....................................................................
                     Parameters of the simulation
 *)

(* size of the world, in arbitrary units of a "block" *)
let cX_DIMENSION = 200 ;;
let cY_DIMENSION = 200 ;;

(* number of time steps that the simulation runs *)
let cTIME_STEPS = 1000 ;;
let cPAUSE_EVERY = max_int ;;

(* number of simulated persons *)
let cPOPULATION = 1000 ;;
(* proportion of initial population that is infected *)
let cINITPROPORTION = 1.0 /. 50.0 ;;

(* the number of blocks in any direction that a person can move at
   each time step, depending on infection status *)
let cSTEP_SIZE_SUSCEPTIBLE = 1 ;;
let cSTEP_SIZE_INFECTED = cSTEP_SIZE_SUSCEPTIBLE ;;
let cSTEP_SIZE_RECOVERED = cSTEP_SIZE_SUSCEPTIBLE ;;
let cSTEP_SIZE_DECEASED = 0 ;; (* should be 0, unless...*zombies*! *)

(* the incremental probability that being within the neighbor radius
   of a person will cause infection *)
let cINFECTIOUSNESS_SUSCEPTIBLE = 0.0 ;;
let cINFECTIOUSNESS_INFECTED = 0.2 ;;
let cINFECTIOUSNESS_RECOVERED = 0.0 ;;
let cINFECTIOUSNESS_DECEASED = 0.0 ;; (* again, ...*zombies*! *)
      
(* distance in blocks that an infected person can infect others *)
let cNEIGHBOR_RADIUS = 4 ;;
(* proportion of infected that die on any given time step *)
let cMORTALITY = 2. /. 100. ;;
(* mean and stdev of time steps for infected to recover (or die) *)
let cRECOVERY_PERIOD = 50., 20. ;;
(* mean and stdev of time steps during which recovered are immune *)
let cIMMUNITY_PERIOD = 100., 40. ;;

(*....................................................................
                   Parameters of the visualization
 *)

(* flag determines if using visualization or not *)
let cVISUALIZE = ref true ;;
  
(* minimum time between displaying successive frames *)
let cFRAME_DELAY = 1. /. 60. ;;
(* number of steps of the simulation per rendered frame *)
let cUPDATES_PER_FRAME = 5 ;;
(* size of each block in pixels *)
let cPIXELS_PER_BLOCK = 3 ;;
(* radius of each symbol in pixels *)
let cSYMBOL_SIZE = 4 ;;
  
(* colors for depicting different states *)
let cCOLOR_SUSCEPTIBLE = 0x5888c4 ;;
let cCOLOR_INFECTED = 0xc4586f ;;
let cCOLOR_RECOVERED = 0x686061 ;;
let cCOLOR_DECEASED = 0xc1bab8 ;;

(* chart placement in pixels *)
let cCHART_X = 50
let cCHART_Y = 100
let cCHART_WIDTH = cX_DIMENSION * cPIXELS_PER_BLOCK - 2 * cCHART_X
let cCHART_HEIGHT = cY_DIMENSION * cPIXELS_PER_BLOCK - 2 * cCHART_Y

let cCOUNT_X = cX_DIMENSION * cPIXELS_PER_BLOCK + 50
let cCOUNT_Y = 50
