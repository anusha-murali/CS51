(*
                         CS 51 Problem Set 3
                           Bignums and RSA
*)

(* Your work will be in the file `bignum.ml`. However, feel free to
take a look at this file, which uses the functions from `bignum.ml` to
implement RSA encryption.

We start by performing an `open` on the `bignum` module that you've
built. *)

(* open Unix;; *)
(* open Bignum ;; *)

(* Then, we add some additional bignum functions and the RSA
implementation. *)
  
(*======================================================================
  More powerful arithmetic functions on bignums 
 *)
  
(* div_single b n -- Divides a bignum `b` (actually, just the
   coefficients part) by a single "digit" `n`, that is, returns
   coefficients for a bignum representing b/n, where n is an integer
   less than cBASE, along with the remainder b mod n *)
let div_single (b : int list) (n : int) : int list * int =
  
  let rec div_single_rec (b : int list) (r : int) : int list * int =
    match b with
    | [] -> [], r
    | h :: t ->
        let dividend = r * cBASE + h in
        let quot = dividend / n in
        let (q, r) = div_single_rec t (dividend-quot * n) in
        (quot :: q, r) in
  
  match b with
  | [] -> [], 0
  | [a] -> [a / n], a mod n
  | h1 :: h2 :: t -> if h1 < n then div_single_rec (h1 * cBASE + h2 :: t) 0
                     else div_single_rec b 0 ;;

(* div_mod b1 b2 -- Returns a pair (floor of b1/b2, b1 mod b2), both
   of which are bignums *)
let div_mod (b1 : bignum) (b2 : bignum) : bignum * bignum =
  
  let rec div_mod_rec m n (psum : bignum) : bignum * bignum =
    if less m n then (psum, m)
    else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
      | [] -> failwith "Division by zero"
      | ns :: _ -> let (p, _) =
          if ns + 1 = cBASE then
            (take_first mc (List.length mc - List.length nc), 0)
          else
            let den = ns + 1 in
            let num = take_first mc (List.length mc - List.length nc + 1) in
            div_single num den
        in
        let bp = clean {neg = false; coeffs = p} in
        let p2 = clean (if equal bp (from_int 0) then from_int 1 else bp) in
        div_mod_rec (clean (plus m (negate (times n p2))))
                    (clean n)
                    (clean (plus psum p2)) in
  
  div_mod_rec (clean b1) (clean b2) (from_int 0) ;;

(* exp_mod b e m -- Returns `b` to the power of `e mod m` *)
let rec exp_mod (b : bignum) (e : bignum) (m : bignum) : bignum =
  if equal e (from_int 0) then from_int 1
  else if equal e (from_int 1) then
    snd (div_mod (clean b) (clean m))
  else
    let (q, r) = div_mod (clean e) (from_int 2) in
    let res = exp_mod (clean b) q (clean m) in
    let (_, x) = div_mod (times (times res res) (exp_mod (clean b) r (clean m)))
                        (clean m) in
    {neg = x.neg; coeffs = trim_leading_zeroes x.coeffs} ;;

(* exponent b e -- Returns `b` to the power of `e` *)
let rec exponent (b : bignum) (e : bignum) : bignum =
  if equal (clean e) (from_int 0) then from_int 1
  else if equal (clean e) (from_int 1) then clean b
  else
    let (q, r) = div_mod (clean e) (from_int 2) in
    let res = exponent (clean b) q in
    let exp = (times (times res res) (exponent (clean b) r))
    in {neg = exp.neg; coeffs = trim_leading_zeroes exp.coeffs} ;;

(* is_prime `b` -- Returns true if bignum `b` is prime, false
   otherwise, using the Rabin-Miller primality testing algorithm. *)
let is_prime (n : bignum) : bool =
  
  let rec miller_rabin (k : int) (d : bignum) (s : int) : bool =
    let rec square (r : int) (x : bignum) =
      if r >= s then false else
        let x = exp_mod x (from_int 2) n in
        
        if equal x (from_int 1) then false
        else if equal x (plus n (from_int (~-1))) then miller_rabin (k - 1) d s
        else square (r + 1) x in
    
    if k < 0 then true
    else      
      let a = plus (rand_bignum (plus n (from_int (~-4)))) (from_int 2) in
      let x = exp_mod a d n in
      if equal x (from_int 1) || equal x (plus n (from_int (~-1))) then
        miller_rabin (k - 1) d s
      else square 1 x in
  
  (* factor powers of 2 to return (d, s) such that n = (2^s) * d *)
  let rec factor (n : bignum) (s : int) =
    let (q, r) = div_mod n (from_int 2) in
    if equal r (from_int 0) then factor q (s + 1) else (n, s) in
  
  let (_, r) = div_mod n (from_int 2) in
  if equal r (from_int 0) then false else
    let (d, s) = factor (plus n (from_int (~-1))) 0 in
    miller_rabin 20 d s ;;
  
(* euclid m d -- Returns (s, t, g) such that g is gcd(m, d) and
   s*m + t*d = g; uses Euclid's algorithm for GCD *)
let rec euclid (m : bignum) (d : bignum) : bignum * bignum * bignum =
  if equal d (from_int 0) then (from_int 1, from_int 0, m)
  else
    let (q, r) = div_mod m d in
    let (s, t, g) = euclid d r in
    (clean t, clean (plus s (negate (times q t))), clean g) ;;

(* generate_random_prime min max -- Generate a random prime number
   between min and max-1 (inclusive) *)
let rec generate_random_prime (min : bignum) (max : bignum) : bignum =
  let rand = plus (rand_bignum (plus max (negate min))) min in
  if is_prime rand then rand
  else generate_random_prime min max ;;
                                          
(*======================================================================
  The RSA Cryptosystem
 *)

(* generate_key_pair r -- Generate a random RSA key pair, returned as
   (e, d, n).  p and q will be between 2^r and 2^(r+1).  Recall that
   (n, e) is the public key, and (n, d) is the private key. *)
  
let rec generate_key_pair (r : bignum) : bignum * bignum * bignum =
  (* constants for 1 and 2 *)
  let c1 = from_int 1 in
  let c2 = from_int 2 in
  (* p and q are random primes in the right range *)
  let p = generate_random_prime (exponent c2 r) (exponent c2 (plus r c1)) in
  let q = generate_random_prime (exponent c2 r) (exponent c2 (plus r c1)) in
  (* m = (p - 1)(q - 1) *)
  let m = times (plus p (negate c1)) (plus q (negate c1)) in
  (* find an e relatively prime to m, returning e, d, and n *)
  let rec selectPair () =
    let e = generate_random_prime (exponent c2 r) (exponent c2 (plus r c1)) in
    let (_, d, g) = euclid m e in
    let d = if d.neg then plus d m else d in
      if equal g c1 then (clean e, clean d, clean (times p q))
      else selectPair ()
  in
    if equal p q then generate_key_pair r else selectPair () ;;

(*----------------------------------------------------------------------
  Encrypting and decrypting bignums
 *)

(* encrypt_decrypt_bignum n e_or_d s -- Encrypt or decrypt a bignum
   `s` with modulus `n`. To encrypt, pass in the argument `e_or_d`
   should be the encryption key e. To decrypt, the argument `e_or_d`
   should be the decryption key d. *)
let encrypt_decrypt_bignum (n : bignum)
                           (e_or_d : bignum)
                           (s : bignum)
                         : bignum =
  exp_mod s e_or_d n ;;

(* chars_to_bignums lst m -- Pack a list of chars `lst` as a list of
   bignums, with `m` chars to a bignum. *)
let rec chars_to_bignums (lst : char list) (m : int) : bignum list =

  let rec encchars lst =
    match lst with
    | [] -> (from_int 0)
    | c :: t -> clean (plus (times (encchars t) (from_int 256))
                            (from_int (int_of_char c))) in
  
    match lst with
    | [] -> []
    | _ -> let (enclist, rest) = split lst m in
           encchars enclist :: chars_to_bignums rest m

(* bignums_to_chars lst m -- Unpack a list of bignums `lst` into chars
   (inverse of chars_to_bignums) *)
let rec bignums_to_chars (lst : bignum list) : char list =

  let rec decbignum b =
    if equal b (from_int 0) then []
    else let (q, r) = div_mod b (from_int 256) in
      match to_int r with
      | None -> failwith "bignums_to_chars: representation invariant broken"
      | Some ir -> char_of_int ir :: decbignum q in
  
  match lst with
  | [] -> []
  | b :: t -> decbignum b @ bignums_to_chars t
                                             
(* bytes_in_key n -- Return the number of bytes required to represent
   an RSA modulus `n` *)
let bytes_in_key (n : bignum) =
  int_of_float (float_of_int (List.length (trim_leading_zeroes n.coeffs) - 1)
                *. log10 (float_of_int cBASE) /. (log10 2. *. 8.)) ;;

(* encrypt_decrypt_bignums n e lst -- Encrypts or decrypts a list of
   bignums using RSA. To encrypt, pass in n e lst. To decrypt, pass in
   n d lst. *)
let encrypt_decrypt_bignums (n : bignum) (e_or_d : bignum)
                            (lst : bignum list)
                          : bignum list =
  List.map (encrypt_decrypt_bignum n e_or_d) lst ;;

(* encrypt n e s -- Encrypts string s using encryption key (n, e) *)
let encrypt (n : bignum) (e : bignum) (s : string) =
  encrypt_decrypt_bignums n e (chars_to_bignums (explode s)
                                                (bytes_in_key n)) ;;

(* encrypt n e s -- Decrypts string s using decryption key (n, d) *)
let decrypt (n : bignum) (d : bignum) (m : bignum list) =
  implode (bignums_to_chars (encrypt_decrypt_bignums n d m)) ;;  
