(*
                         CS 51 Problem Set 3
                           Bignums and RSA

                                Puzzle

In this file, we use the RSA module (`rsa.ml`) to decrypt a secret
message. If your bignum implementation in `bignum.ml` is working
properly, compiling and running this file should display an
appropriate message.
 *)

open Bignum ;;
open Rsa ;;

(* These are the keys *)
  
let cE = {neg = false; coeffs = [8; 784; 761]} ;;
let cD = {neg = false; coeffs = [67; 22; 243; 567; 777]} ;;
let cN = {neg = false; coeffs = [172; 282; 167; 939; 367]} ;;

(* The mystery ciphertext *) 

let ciphertext = [{neg = false; coeffs = [126; 779; 384; 686; 909]};
                  {neg = false; coeffs = [82; 150; 628; 523; 894]};
                  {neg = false; coeffs = [153; 685; 976; 505; 45]};
                  {neg = false; coeffs = [27; 91; 70; 991; 631]};
                  {neg = false; coeffs = [78; 467; 199; 813; 346]};
                  {neg = false; coeffs = [88; 609; 616; 129; 797]};
                  {neg = false; coeffs = [94; 194; 208; 972; 919]};
                  {neg = false; coeffs = [156; 989; 62; 246; 355]}] ;;

(* RSA decrypting the ciphertext, which exercises the RSA
   implementation and the bignum implementation it's based on *)

let m = decrypt cN cD ciphertext in
  Printf.printf "The secret message is /%s/\n" m ;;
