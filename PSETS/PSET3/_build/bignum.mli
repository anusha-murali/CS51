(*
                         CS 51 Problem Set 3
                      Bignums and RSA encryption

                          Bignums interface
*)

type bignum = {neg: bool; coeffs: int list}

val cBASE : int

val negate : bignum -> bignum
val equal : bignum -> bignum -> bool
val less : bignum -> bignum -> bool
val greater : bignum -> bignum -> bool
val from_int : int -> bignum
val to_int : bignum -> int option

val trim_leading_zeroes : int list -> int list
val clean : bignum -> bignum
val rand_bignum : bignum -> bignum
val explode : string -> char list
val implode : char list -> string
val take_first : 'a list -> int -> 'a list
val split : 'a list -> int -> 'a list * 'a list
val intlog : int -> int
  
val from_string : string -> bignum
val to_string : bignum -> string
                            
val plus : bignum -> bignum -> bignum
val times : bignum -> bignum -> bignum

val times_faster : bignum -> bignum -> bignum

val minutes_spent_on_pset : unit -> int
val reflection : unit -> string
