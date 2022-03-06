val frac_ops :
  (string * (Fraction.frac -> Fraction.frac -> Fraction.frac)) list

val int_ops : (string * (int -> int -> int)) list
val float_ops : (string * (float -> float -> float)) list

type 'a token

val opnd : 'a token -> 'a
(** [opnd tok] is the operand represented by token. Raises: [Failure]
    when tok is not an operand*)

val eval_postfix :
  (string * ('a -> 'a -> 'a)) list -> 'a token list -> 'a
(** [eval_postfix rules tokens] evaluates list of [tokens] using
    operations specified in [rules]. Example: 12+34+* should evaluate to
    21. Raises: [Failure] when [tokens] does not constitute a valid
    postfix expression *)
