(*
                         CS 51 Problem Set 6
                                Search
 
                               Drawing

  Basic functionality for drawing using OCaml's graphics module.
 *)

module G = Graphics ;;

let _ = G.auto_synchronize false

(* Constants for drawing. *)
let cLINEWIDTH = 3 ;;
let cTILEWIDTH = 100 ;;
let cFRAMESIZE = 500 ;;
let cFRAMEDELAY = 0.1 ;;

(* Colors used for drawing. *)
let cWALLCOLOR = 0x993333 ;;
let cLOCCOLOR = 0xAD42F4 ;;
let cGOALBGCOLOR = 0xAD42F4 ;;
let cGOALCOLOR = 0x808080 ;;
let cUNSEENCOLOR = 0xd9d9d9 ;;
let cHILITECOLOR = 0xc2c2a3 ;;

(* Drawing simple shapes*)
let draw_square (c : G.color) (y : int) (x : int) (w : int) (h : int) : unit =
  G.set_color c;
  G.fill_rect (x * w) (cFRAMESIZE - h - y * h) w h ;;

let draw_small_square (c : G.color) (y : int) (x : int) (w : int) (h : int) 
                    : unit =
  G.set_color c;
  G.fill_rect (x * w + w * 1 / 8) 
              (cFRAMESIZE - h - y * h + h * 1 / 8) 
              (w * 3 / 4) 
              (h * 3 / 4) ;;

let draw_circle (c : G.color) (y : int) (x : int) (w : int) (h : int) : unit =
  G.set_color c;
  G.fill_ellipse (w / 2 + x * w)
                 (cFRAMESIZE - h / 2 - y * h)
                 (w / 2) (h / 2) ;;

(* Delay between frames *)
let rec delay (sec : float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec ;;

