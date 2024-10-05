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
         (fun () -> Cons(Tone(0.0, pitch, volume),
                         (fun () -> Cons(Stop(duration, pitch),
                                         list_to_stream_aux tl))))
       | Rest duration -> 
         (fun () -> Cons(Tone(0.0, (C,0), 0),
                         (fun () -> Cons(Stop(duration, (C,0)),
                                         list_to_stream_aux tl)))))
  in list_to_stream_aux lst ;;


let rec list_to_stream (lst : obj list) : event NLS.stream =
  let rec list_to_stream_aux remaining =
  match remaining with
    | [] -> list_to_stream lst
    | hd :: tl ->
      (match hd with
       | Note (pitch, duration, volume) ->
         lazy (Cons(Tone(0.0, pitch, volume), lazy (Cons(Stop(duration, pitch),
                                         list_to_stream_aux tl))))
       | Rest duration ->
         lazy (Cons(Tone(0.0, (C,0), 0), lazy (Cons(Stop(duration, (C,0)),
                                         list_to_stream_aux tl)))))
  in list_to_stream_aux lst ;;

