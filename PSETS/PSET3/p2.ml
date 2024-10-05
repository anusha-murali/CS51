(*
                         CS 51 Problem Set 3
                           Bignums and RSA

                                Puzzle

In this file, we use the RSA module (`rsa.ml`) to decrypt a secret
message. If your bignum implementation in `bignum.ml` is working
properly, compiling and running this file should display an
appropriate message.
 *)

(* These are the keys *)
  
let cE = {neg = false; coeffs = [5]} ;;
let cD = {neg = false; coeffs = [3]} ;;
let cN = {neg = false; coeffs = [7]} ;;

(* The mystery ciphertext *) 

let ciphertext = [{neg = false; coeffs = [126]}] ;;

(* RSA decrypting the ciphertext, which exercises the RSA
   implementation and the bignum implementation it's based on *)

let m = decrypt cN cD ciphertext in
  Printf.printf "The secret message is /%s/\n" m ;;
