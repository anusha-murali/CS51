type binop = Add | Sub | Mul | Div | Pow
type unop = Sin | Cos | Ln | Neg
type expression =
    Num of float
  | Var
  | Binop of binop * expression * expression
  | Unop of unop * expression
exception ParseError of string
type token =
    NumT of float
  | VarT
  | BinopT of binop
  | UnopT of unop
  | LParen
  | RParen
  | LBrace
  | RBrace
  | EOF
val recognized_tokens : string array
val token_expressions : token array
val string_to_char_list : string -> char list
val is_digit : char -> bool
val binop_precedence : binop -> int
val unop_precedence : unop -> int
val prec_bound : int
val binop_is_associative : binop -> bool
val binop_to_string : binop -> string
val unop_to_string : unop -> string
val token_to_string : token -> string
val to_string_smart : expression -> string
val to_string : expression -> string
val match_while : (char -> bool) -> char list -> string * char list
val lex_number_string : char list -> string * char list
val lex_number : char list -> (token * char list) option
val match_string : char list -> string -> char list option
val lex_multi_char_token : char list -> (token * char list) option
val lex' : char list -> token list
val lex : string -> token list
val parse : string -> expression
val rand_exp : int -> expression
val rand_exp_str : int -> string
