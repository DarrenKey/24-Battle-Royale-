val frac_rules :
  (string * (Fraction.frac -> Fraction.frac -> Fraction.frac)) list

val int_rules : (string * (int -> int -> int)) list
val float_rules : (string * (float -> float -> float)) list

type 'a token =
  | Operand of 'a
  | Operator of string

val to_tokens : 'a list -> 'a token list
(** [to_tokens lst] is [lst] of operands converted to tokens *)

val ops_of_rules : (string * ('a -> 'a -> 'a)) list -> string list

val opnd : 'a token -> 'a
(** [opnd tok] is the operand represented by token. Raises: [Failure]
    when tok is not an operand*)

val eval_postfix :
  (string * ('a -> 'a -> 'a)) list -> 'a token list -> 'a
(** [eval_postfix rules tokens] evaluates list of [tokens] using
    operations specified in [rules]. Example: 12+34+* should evaluate to
    21. Raises: [Failure] when [tokens] does not constitute a valid
    postfix expression *)
