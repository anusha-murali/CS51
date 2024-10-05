(*
                         CS 51 Problem Set 3
                           Bignums and RSA

                            RSA Interface
*)

open Bignum ;;
  
val generate_key_pair : bignum -> bignum * bignum * bignum

val encrypt_decrypt_bignum : bignum -> bignum -> bignum -> bignum
val encrypt : bignum -> bignum -> string -> bignum list
val decrypt : bignum -> bignum -> bignum list -> string
