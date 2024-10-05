(* 
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                            Part 3: Music
 *) 

module NLS = NativeLazyStreams ;;

exception InvalidHex ;;
exception InvalidPitch ;;

(*----------------------------------------------------------------------
                    Music data types and conversions
 *)

(* Pitches within an octave *)
type p = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab ;;

(* Pitches with octave *)
type pitch = p * int ;;

(* Musical objects *)              
type obj =
  | (* notes with a pitch, duration (float; 1.0 = a measure), and
       volume ([0...128]) *)
    Note of pitch * float * int
  | (* rests with a duration only *)
    Rest of float ;;

(*......................................................................
Some functions that may be useful for quickly creating notes to test
and play with. *)

(* half pitch -- Returns a note of the given `pitch` that is a half of a
   measure long *)
let half (pt : pitch) : obj = Note (pt, 0.5, 60) ;; 

(* quarter pitch -- Returns a note of the given `pitch` that is a
   quarter of a measure long *)
let quarter (pt : pitch) : obj = Note (pt, 0.25, 60) ;; 

(* eighth pitch -- Returns a note of the given `pitch` that is an eighth
   of a measure long *)
let eighth (pt : pitch) : obj = Note (pt, 0.125, 60) ;;

(* quarter_rest -- A rest that is a quarter of a measure *)
let quarter_rest : obj = Rest 0.25 ;;

(* eighth_rest -- A rest that is an eighth of a measure *)
let eighth_rest : obj = Rest 0.125 ;;
  
(*......................................................................
            Event representation of note and rest sequences
 *)
type event =
  (* start to play a note after the given time (a float, interpreted 
     as relative to the previous event) and volume (int; [0..128]) *)
  | Tone of float * pitch * int
  (* stop playing the note with the given pitch after the given time
     (a float, interpreted as relative to the previous event) *)
  | Stop of float * pitch ;;          

(* p_to_int p -- Converts pitch `p` to an integer (half-step)
   representation *)
let p_to_int (p : p) : int =
  match p with
  | C  -> 0 | Db -> 1 | D  -> 2 | Eb -> 3 | E  ->  4 | F ->  5
  | Gb -> 6 | G  -> 7 | Ab -> 8 | A  -> 9 | Bb -> 10 | B -> 11 ;;

(* int_to_p i -- Converts integer `i` (interpreted as a half step
   relative to C, to a pitch *)
let int_to_p : int -> p =
  let pitches = [C; Db; D; Eb; E; F; Gb; G; Ab; A; Bb; B] in
  fun n -> if n < 0 || n > 11 then raise InvalidPitch
           else List.nth pitches n ;;

(* time_of_event e -- Given an event `e`, returns at what relative
   time it occurs *)
let time_of_event (event : event) : float =
  match event with
  | Tone (time, _, _)
  | Stop (time, _) -> time ;;

(* shift by e -- Returns an event like `e` but with time shifted
   so that it occurs later by `offset` *)
let shift (offset : float) (event : event) : event =
  match event with
  | Tone (time, pitch, vol) -> Tone (time +. offset, pitch, vol)
  | Stop (time, pitch)      -> Stop (time +. offset, pitch) ;;

(* shift_start offset str -- Shifts the start of a stream of events
   `str` so that it begins later by `offset`. Since event times are
   relative, only the first event needs to be modified. *)
let shift_start (offset : float) (str : event NLS.stream)
              : event NLS.stream =
  let NLS.Cons (event, remaining) = Lazy.force str in
  lazy (NLS.Cons (shift offset event, remaining)) ;;

(*......................................................................
                         Generating MIDI output
 *)

(* hex_to_int hex -- Converts a `hex` number in string representation
   to an `int` *)
let hex_to_int (hex : string) : int = int_of_string ("0x" ^ hex) ;;

(* int_to_hex n -- Converts an int `n` to a hex number in string
   representation *)
let int_to_hex (n : int) : string = Printf.sprintf "%02x" n ;;

(* output_hex outchan hex -- Outputs a string `hex` (intended to
   specify a hex value) on the specified output channel `outchan` *)
let rec output_hex (outchan : out_channel) (hex : string) : unit =
  let len = String.length hex in
  if len = 0 then ()
  else if len < 2 then raise InvalidHex
  else (output_byte outchan (hex_to_int (String.sub hex 0 2)); 
        output_hex outchan (String.sub hex 2 (len - 2))) ;;

(* Some MIDI esoterica *)
  
let cTICKS_PER_Q = 32 ;;
  
let cHEADER = "4D546864000000060001000100"
              ^ (int_to_hex cTICKS_PER_Q)
              ^ "4D54726B" ;;

let cFOOTER = "00FF2F00" ;;

(* pitch_to_hex pitch -- Convert a `pitch` to a string of its hex
   representation *)
let pitch_to_hex (pitch : pitch) : string =
  let (p, oct) = pitch in
  int_to_hex ((oct + 1) * 12 + (p_to_int p)) ;;

(* time_to_hex time -- Convert an amount of `time` to a string of its
   hex representation *)
let time_to_hex (time : float) : string =
  let measure = cTICKS_PER_Q * 4 in
  let itime = int_of_float (time *. (float measure)) in
  if itime < measure then (int_to_hex itime)
  else "8" ^ (string_of_int (itime / measure))
       ^ (Printf.sprintf "%02x" (itime mod measure)) ;;

(* stream_to_hex n str -- Converts the first `n` events of a stream
   `str` of music to a string hex representation *)
let rec stream_to_hex (n : int) (str : event NLS.stream) : string =
  if n = 0 then ""
  else match Lazy.force str with
       | NLS.Cons (Tone (t, pitch, vol), tl) -> 
          (time_to_hex t) ^ "90" ^ (pitch_to_hex pitch)
          ^ (int_to_hex vol) ^ (stream_to_hex (n - 1) tl)
       | NLS.Cons (Stop (t, pitch), tl) ->
          (time_to_hex t) ^ (pitch_to_hex pitch) ^ "00"
          ^ (stream_to_hex (n - 1) tl) ;;
              
(* output_midi file hex -- Writes the `hex` string representation of
   music to a midi file called `filename` *)
let output_midi (filename : string) (hex : string) : unit =
  let outchan = open_out_bin filename in
  output_hex outchan cHEADER; 
  output_binary_int outchan ((String.length hex) / 2 + 4); 
  output_hex outchan hex; 
  output_hex outchan cFOOTER; 
  flush outchan; 
  close_out outchan ;;

(*----------------------------------------------------------------------
             Conversion to and combination of music streams
 *)
  
(*......................................................................
Problem 8. Write a function `list_to_stream` that builds a music
stream from a finite list of musical objects. The stream should repeat
this music forever. (In order for the output to be well defined, the
input list must have at least one note. You can assume as much.) Hint:
Use a recursive auxiliary function `list_to_stream_aux` as shown
below, which will call itself recursively on the list allowing you to
keep keep the original list around as well. Both need to be recursive,
since you will call both the inner and outer functions at some
point. See below for some examples.
......................................................................*)
let rec list_to_stream (lst : obj list) : event NLS.stream =
  let rec list_to_stream_aux remaining =
  match remaining with
    | [] -> list_to_stream lst
    | hd :: tl ->
      (match hd with
       | Note (pitch, duration, volume) ->
         lazy (NLS.Cons(Tone(0.0, pitch, volume), lazy (NLS.Cons(Stop(duration, pitch),
                                         list_to_stream_aux tl))))
       | Rest duration ->
         lazy (NLS.Cons(Tone(0.0, (C,0), 0), lazy (Cons(Stop(duration, (C,0)),
                                         list_to_stream_aux tl)))))
  in list_to_stream_aux lst ;;

(*......................................................................
Problem 9. Write a function `pair` that merges two event streams. Events
that happen earlier in time should appear earlier in the merged
stream. See below for some examples.
......................................................................*)
let rec pair (a : event NLS.stream) (b : event NLS.stream)
           : event NLS.stream =
  let ha = NLS.head a in
  let hb = NLS.head b in
  if (time_of_event ha) = (time_of_event hb)
  then lazy (Cons(ha, lazy (Cons(shift ~-.(time_of_event ha) hb,
                                         (pair (NLS.tail a)  (NLS.tail b))))))
  else if ((time_of_event ha) < (time_of_event hb))
       then
         lazy (Cons(ha, (pair (NLS.tail a) (shift_start ~-.(time_of_event ha) b))))
       else 
         lazy (Cons(hb, (pair (NLS.tail b) (shift_start ~-.(time_of_event hb) a))))
;;

(*......................................................................
Problem 10. Write a function `transpose` that takes an event stream and
moves each pitch up by `half_steps` pitches. Note that `half_steps` can be
negative, but this case is particularly difficult to reason about so
we've implemented it for you. See below for some examples.
......................................................................*)
let transpose_pitch ((p, oct) : pitch) (half_steps : int) : pitch =
  let newp = (p_to_int p) + half_steps in
  if newp < 0 then
    if newp mod 12 = 0 then (C, oct + (newp / 12))
    else (int_to_p (newp mod 12 + 12), oct - 1 + (newp / 12))
  else (int_to_p (newp mod 12), oct + (newp / 12))

let transpose (str : event NLS.stream) (half_steps : int)
            : event NLS.stream =
 let aux x =
    match x with
    | Tone(f, p,i) -> Tone(f, (transpose_pitch  p half_steps), i)
    | Stop(f, p) -> Stop(f, (transpose_pitch p half_steps)) 
 in
 NLS.smap (fun x -> aux x) str
;;

(*----------------------------------------------------------------------
                         Testing music streams
 *)

(* ... UNCOMMENT THIS SECTION ONCE YOU'VE IMPLEMENTED 
                 THE FUNCTIONS ABOVE. ... *)

(*......................................................................
For testing purposes, let's start with a trivial example, useful for
checking list_to_stream, transpose, and pair functions. Start with a
simple melody1: *)

let melody1 = list_to_stream [quarter (C,3);
                              quarter_rest;
                              half (E,3)] ;;

(* This melody, when converted to a stream of start and stop events,
should look something like this:

    # NLS.first 5 melody1 ;;
    - : event list =
    [Tone (0., (C, 3), 60); Stop (0.25, (C, 3)); Tone (0.25, (E, 3), 60);
     Stop (0.5, (E, 3)); Tone (0., (C, 3), 60)]

Now, we transpose it and shift the start forward by a quarter note: *)
  
let melody2 = shift_start 0.25
                          (transpose melody1 7) ;;

(* The result is a stream that begins as

s    # NLS.first 5 melody2 ;;
    - : event list =
    [Tone (0.25, (G, 3), 60); Stop (0.25, (G, 3)); Tone (0.25, (B, 3), 60);
     Stop (0.5, (B, 3)); Tone (0., (G, 3), 60)]

Finally, combine the two as a harmony: *)
  
let harmony = pair melody1 melody2 ;;

(* The result begins like this:

    # NLS.first 10 harmony ;;
    - : event list =
    [Tone (0., (C, 3), 60); Tone (0.25, (G, 3), 60); Stop (0., (C, 3));
     Stop (0.25, (G, 3)); Tone (0., (E, 3), 60); Tone (0.25, (B, 3), 60);
     Stop (0.25, (E, 3)); Tone (0., (C, 3), 60); Stop (0.25, (B, 3));
     Tone (0., (G, 3), 60)]

You can write this out as a midi file and listen to it. *)
                              
let _ = output_midi "temp.mid" (stream_to_hex 16 harmony) ;;
   
   
(*......................................................................
The next example combines some scales. Uncomment these lines when you're
done implementing the functions above. You can listen
to it by opening the file "scale.mid". *)


let scale1 = list_to_stream (List.map quarter
                                      [(C,3); (D,3); (E,3); (F,3); 
                                       (G,3); (A,3); (B,3); (C,4)]) ;;

let scale2 = transpose scale1 7 ;; 

let scales = pair scale1 scale2 ;; 

let _ = output_midi "scale.mid" (stream_to_hex 32 scales) ;; 


(*......................................................................
Problem 11: Then with just three lists provided after this comment and
the functions we defined, produce (a small part of) a great piece of
music. The piece should be four streams merged: one should be the bass
playing continuously from the beginning. The other three should be the
melody, starting 2, 4, and 6 measures from the beginning,
respectively.

Define a stream `canon` here using the above component
streams `bass` and `melody`. Uncomment the definitions above and the
lines below when you're done. Run the program and open "canon.mid" to
hear the beautiful music.
......................................................................*)
   
let bass = list_to_stream
              (List.map quarter [(D, 3); (A, 2); (B, 2); (Gb, 2); 
                                 (G, 2); (D, 2); (G, 2); (A, 2)]) ;; 

let slow = [(Gb, 4); (E, 4); (D, 4); (Db, 4); 
            (B, 3); (A, 3); (B, 3); (Db, 4);
            (D, 4); (Db, 4); (B, 3); (A, 3);
            (G, 3); (Gb, 3); (G, 3); (E, 3)] ;;

let fast = [(D, 3); (Gb, 3); (A, 3); (G, 3);
            (Gb, 3); (D, 3); (Gb, 3); (E, 3); 
            (D, 3); (B, 2); (D, 3); (A, 3);
            (G, 3); (B, 3); (A, 3); (G, 3)] ;; 

let melody = list_to_stream ((List.map quarter slow)
                             @ (List.map eighth fast));;
let canon = lazy (failwith "canon not implemented") ;;

(* output_midi "canon.mid" (stream_to_hex 176 canon);; *)

 
(*......................................................................
Problem 12: Challenge problem. There's lots of opportunity for
extending your system. If you want, try implementing other functions
for manipulating event streams. Maybe one that increases or decreases
the timescale, or one that doubles every note, or increases or
decreases the volume. See what kind of music you can create. We've
given you some more interesting streams of music to play around with
if you'd like, named `part1` through `part4`. They're all the same
length, 2.25 measures. Try overlaying them all and outputting it as a
midi file. Or just make your own music streams here.
......................................................................*)
   
(*
let part1 = list_to_stream
              [Rest 0.5;  Note((D, 4), 0.75, 60);  
               Note((E, 4), 0.375, 60); Note((D, 4), 0.125, 60);  
               Note((B, 3), 0.25, 60); Note((Gb, 3), 0.1875, 60);  
               Note((G, 3), 0.0625, 60)] ;; 
  
let part2 = list_to_stream
              [Note((G, 3), 0.1875, 60); Note((A, 3), 0.0625, 60); 
               Note((B, 3), 0.375, 60); Note((A, 3), 0.1875, 60); 
               Note((B, 3), 0.0625, 60); Note((C, 4), 0.5, 60); 
               Note((B, 3), 0.5, 60)] ;; 

let part3 = list_to_stream
              [Note((G, 3), 1., 60); Note((G, 3), 0.5, 60); 
               Note((E, 3), 0.1875, 60);
               Note((Gb, 3), 0.0625, 60); Note((G, 3), 0.25, 60); 
               Note((E, 3), 0.25, 60)] ;;

let part4 = list_to_stream
              [Rest(0.25); Note((G, 3), 0.25, 60); 
               Note((Gb, 3), 0.25, 60); Note((E, 3), 0.375, 60);
               Note((D, 3), 0.125, 60); Note((C, 3), 0.125, 60);
               Note((B, 2), 0.125, 60); Note((A, 2), 0.25, 60);
               Note((E, 3), 0.375, 60); Note((D, 3), 0.125, 60)] ;;
 *)
    
(*======================================================================
Reflection on the problem set

     Please fill out the information about time spent and your
     reflection thereon in the file `refs.ml`.
 *)
