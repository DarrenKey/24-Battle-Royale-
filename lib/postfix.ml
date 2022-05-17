let frac_rules =
  [
    ("+", Fraction.add_frac);
    ("-", Fraction.subtract_frac);
    ("*", Fraction.multiply_frac);
    ("/", Fraction.divide_frac);
  ]

let int_rules =
  [ ("+", ( + )); ("-", ( - )); ("*", ( * )); ("/", ( / )) ]

let float_rules =
  [ ("+", ( +. )); ("-", ( -. )); ("*", ( *. )); ("/", ( /. )) ]

type 'a token =
  | Operand of 'a
  | Operator of string

let opnd tok =
  match tok with
  | Operand n -> n
  | Operator _ -> raise (Failure "opnd: not an operand!")

let push rules stk tok =
  match tok with
  | Operand _ -> tok :: stk
  | Operator op -> (
      match stk with
      | a :: b :: tail -> (
          try Operand ((List.assoc op rules) (opnd b) (opnd a)) :: tail
          with Division_by_zero -> raise (Failure "division by zero"))
      | _ -> raise (Failure ("push: " ^ op)))

let rec eval_tokens_wrapper rules stk tokens =
  match tokens with
  | [] -> (
      match stk with
      | [ a ] -> opnd a
      | _ -> raise (Failure "eval_tokens_wrapper: invalid expression"))
  | h :: tail -> eval_tokens_wrapper rules (h |> push rules stk) tail

let eval_postfix rules tokens = eval_tokens_wrapper rules [] tokens

exception NotValidPostFix

let postfix_to_infix tokens =
  let op_stack = Stack.create () in
  let rec main_loop token_list =
    match token_list with
    | [] -> ()
    | h :: t -> (
        match h with
        | Operand x ->
            Stack.push (string_of_int x) op_stack;
            main_loop t
        | Operator x ->
            let first = Stack.pop op_stack in
            let second = Stack.pop op_stack in
            Stack.push ("(" ^ second ^ x ^ first ^ ")") op_stack;
            main_loop t)
  in
  main_loop tokens;
  Stack.pop op_stack

let rec to_tokens lst =
  match lst with
  | [] -> []
  | h :: tail -> Operand h :: to_tokens tail

let rec ops_of_rules rules =
  match rules with
  | [] -> []
  | h :: tail ->
      (match h with
      | a, b -> a)
      :: ops_of_rules tail
