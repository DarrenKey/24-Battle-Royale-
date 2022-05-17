(** Module to manage postfix representations of an expression. *)

val frac_rules :
  (string * (Fraction.frac -> Fraction.frac -> Fraction.frac)) list
(** frac_rules is an association list that contains pairs or the string
    representation of an operation and the operation itself that is used
    for fractions. *)

val int_rules : (string * (int -> int -> int)) list
(** int_rules is an association list that contains pairs of the string
    representation of an operation and the operation itself that is used
    for integers. *)

val float_rules : (string * (float -> float -> float)) list
(** float_rules is an association list that contains pairs of the string
    representation of an operation and the operation itself that is used
    for floats. *)

(** token is either an Operand or Operator. *)
type 'a token =
  | Operand of 'a
  | Operator of string

val to_tokens : 'a list -> 'a token list
(** [to_tokens lst] is [lst] of operands converted to tokens *)

val ops_of_rules : (string * ('a -> 'a -> 'a)) list -> string list
(** [ops_of_rules rules] is a string list containing all of the first
    elements of each pair in the association list [rules].*)

val opnd : 'a token -> 'a
(** [opnd tok] is the operand represented by token. Raises: [Failure]
    when tok is not an operand *)

val eval_postfix :
  (string * ('a -> 'a -> 'a)) list -> 'a token list -> 'a
(** [eval_postfix rules tokens] evaluates list of [tokens] using
    operations specified in [rules]. Example: 12+34+* should evaluate to
    21. Raises: [Failure] when [tokens] does not constitute a valid
    postfix expression *)

val postfix_to_infix : int token list -> string
(** [postfix_to_infix tokens] converts [tokens] into a string infix
    representation of the tokens. Requires: [tokens] is a valid postfix
    representation. *)
