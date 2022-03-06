(* let op_types = [ ("+", Fraction.add_frac); ("-",
   Fraction.subtract_frac); ("*", Fraction.multiply_frac); ("/",
   Fraction.divide_frac); ] *)

let op_types =
  [ ("+", ( +. )); ("-", ( -. )); ("*", ( *. )); ("/", ( /. )) ]

type 'a token =
  | Operand of 'a
  | Operator of string

let opnd tok =
  match tok with
  | Operand n -> n
  | Operator _ -> raise (Failure "operand: not an operand!")

let push rules stk tok =
  match tok with
  | Operand _ -> tok :: stk
  | Operator op -> (
      match stk with
      | a :: b :: tail ->
          Operand ((List.assoc op rules) (opnd b) (opnd a)) :: tail
      | _ -> raise (Failure ("push: " ^ op)))

let rec eval_tokens_wrapper rules stk tokens =
  match tokens with
  | [] -> (
      match stk with
      | [ a ] -> opnd a
      | _ -> raise (Failure "eval_tokens_wrapper: invalid expression"))
  | h :: tail -> eval_tokens_wrapper rules (h |> push rules stk) tail

let eval_postfix tokens = eval_tokens_wrapper op_types [] tokens