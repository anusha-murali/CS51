(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                      Visualizing the simulation
 *)

open Config ;;
module Stat = Statistics ;;
module G = Graphics ;;
module Utilities = Utilities ;;
  
(*....................................................................
  Timing functions 
 *)

let rec frame_delay : unit -> unit =
  let last_frame_time = ref 0.0 in
  fun () ->
    let current_time = Sys.time () in
    let addl_delay = cFRAME_DELAY -. (current_time -. !last_frame_time) in
    (try
        ignore (Thread.delay addl_delay)
      with
      | Unix.Unix_error _ -> frame_delay ());
    last_frame_time := current_time ;;
  
(*....................................................................
  Key press functions
 *)
let await_key () =
  if !cVISUALIZE then
    ignore (G.read_key ()) ;;

let any_key () =
  if !cVISUALIZE then
    if G.key_pressed () then (await_key (); true)
    else false
  else false

(*....................................................................
  Drawing functions 
 *)

(* draw_circle ?size ?filled x y color -- Draws a circle at location
   (in frame dimension units) corresponding to position `x` and `y`,
   of size (in pixels) `size`, filled if `filled`. *)
let draw_circle ?(size=cSYMBOL_SIZE) ?(filled=true) x y color =
  G.set_color color;
  G.set_line_width 2;
  if filled then
    G.fill_circle (x * cPIXELS_PER_BLOCK) (y * cPIXELS_PER_BLOCK) size
  else
    G.draw_circle (x * cPIXELS_PER_BLOCK) (y * cPIXELS_PER_BLOCK) size ;;

(* draw_cross ?size x y color -- Draws a cross at location (in frame
   dimension units) corresponding to position `x` and `y`, of size (in
   pixels) `size`. *)
let draw_cross ?(size=cSYMBOL_SIZE) x y color =
  G.set_color color;
  G.set_line_width size;
  let framex, framey = x * cPIXELS_PER_BLOCK, y * cPIXELS_PER_BLOCK in
  G.draw_poly_line [| framex - size, framey - size;
                      framex + size, framey + size;
                      framex, framey;
                      framex - size, framey + size;
                      framex + size, framey - size |] ;;

(* initialize () -- Establishes the graphics window and sets its
   properties.*)
let initialize () =
  if !cVISUALIZE then
    begin
      (* open a graphics window to draw into and size it appropriately *)
      G.open_graph "";
      (* resize the window to a 2x1 aspect ratio, allowing for map on
         the left and chart on the right *)
      G.resize_window (cX_DIMENSION * cPIXELS_PER_BLOCK * 2)
                      (cY_DIMENSION * cPIXELS_PER_BLOCK);
      (* turn off auto-synchronizing; we'll handle double buffer
         synchronization ourselves *)
      G.auto_synchronize false;
      G.display_mode false
    end ;;
    
(* render_map objects -- Draws a map of all of the `objects` in the graphics
   window. *)
let render_map objects =
  objects
  |> List.iter (fun obj -> obj#draw) ;;

(* render_chart data colors x y width height -- Draws on the right
   half of the graphics window canvas a stacked bar chart of the
   `data`, which is a list of elements, one for each time step, each
   element containing counts for the different statuses, along with a
   list of `colors`, again one for each status, in the same order. The
   chart is placed with lower left at `x, y` and the given `width` and
   `height`, all in pixels. *)
let render_chart (data : int list list) (colors : G.color list)
                 lowleft_x lowleft_y width height =
  let open List in

  (* shift chart to the right part of the canvas *)
  let lowleft_x = cX_DIMENSION * cPIXELS_PER_BLOCK + lowleft_x in

  (* draw a single stack from `time` with `values` colored as
     `colors` *)
  let draw_stack time values colors =
    assert (length values = length colors);
    let left_edge = lowleft_x
                    + time * width / cTIME_STEPS in

    (* calculate total and (reversed) partial sums, e.g.,
       [3; 5; 9; 1] --> (18, [18; 17; 8; 3]) *)
    let partial_sums =
      List.fold_left (fun (so_far, splits) next ->
                      (so_far + next), (so_far + next) :: splits)
                     (0, []) in
    let total, splits = partial_sums values in
    (* convert split points to pixels from highest to lowest *)
    let splits = List.map
                   (fun sum -> sum * height / total)
                   splits in
    (* draw bars in reverse order from highest to lowest *)
    List.iter2 (fun split color ->
                  G.set_color color;
                  G.moveto left_edge lowleft_y ;
                  let x, y = G.current_point () in
                  G.fill_rect x y 1 split)
               splits
               (List.rev colors) in

  data |> List.iteri (fun time stack -> draw_stack time stack colors) ;;

let render objects counts colors =
  if !cVISUALIZE && Stat.time#count mod cUPDATES_PER_FRAME = 0 then
    begin
      frame_delay ();
      G.clear_graph ();

      (* draw the map of people *)
      render_map objects;

      (* draw the bar chart of state counts *)
      render_chart counts colors
                   cCHART_X cCHART_Y cCHART_WIDTH cCHART_HEIGHT;

      (* list the state counts *)
      G.set_color G.black;
      G.moveto cCOUNT_X cCOUNT_Y;

      let last_counts = List.map string_of_int
                                 (List.hd (List.rev counts)) in
      G.draw_string (String.concat " " last_counts);

      G.synchronize ()
    end ;;
