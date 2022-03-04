type operator =
  | Multiplication
  | Division
  | Addition
  | Subtraction

type tree =
  | Leaf of int
  | Node of operator * tree * tree

let rec check_valid_char (str : string) : bool =
  if str = "" then true
  else
    let s = String.get str 0 in
    match s with
    | '0' .. '9' | '+' | '/' | '-' | 'x' | '*' | '[' | ']' | '(' | ')'
      ->
        String.length str - 1 |> String.sub str 1 |> check_valid_char
    | _ -> false

let rec check_valid_paren (str : string) (paren_stack : char Stack.t) :
    bool =
  if str = "" then Stack.is_empty paren_stack
  else
    let s_first = String.get str 0 in
    let s_rest = String.length str - 1 |> String.sub str 1 in
    match s_first with
    | '[' | '(' ->
        Stack.push s_first paren_stack;
        check_valid_paren s_rest paren_stack
    | ']' ->
        if Stack.is_empty paren_stack then false
        else
          let top_elem = Stack.pop paren_stack in
          if top_elem = '[' then check_valid_paren s_rest paren_stack
          else false
    | ')' ->
        if Stack.is_empty paren_stack then false
        else
          let top_elem = Stack.pop paren_stack in
          if top_elem = '(' then check_valid_paren s_rest paren_stack
          else false
    | _ -> check_valid_paren s_rest paren_stack

let rec check_valid_operations (str : string) (prev_char : char) : bool
    =
  if str = "" then true
  else
    let s_first = String.get str 0 in
    match s_first with
    | '+' | '/' | '-' | 'x' | '*' -> begin
        match prev_char with
        | '0' .. '9' | ']' | ')' -> begin
            try
              let next_char = String.get str 1 in
              match next_char with
              | '0' .. '9' | '[' | '(' ->
                  check_valid_operations
                    (String.length str - 1 |> String.sub str 1)
                    s_first
              | _ -> false
            with Invalid_argument _ -> false
          end
        | _ -> false
      end
    | _ ->
        check_valid_operations
          (String.length str - 1 |> String.sub str 1)
          s_first

let rec check_all_nums_used_once (str : string) (nums : int list) : bool
    =
  if str = "" then
    match nums with
    | [] -> true
    | _ -> false
  else
    let s_first = String.get str 0 in
    match s_first with
    | '0' .. '9' ->
        if String.length str >= 2 then
          try
            let s_two_digit = String.sub str 0 2 |> int_of_string in
            let filtered_nums =
              List.filter (fun s -> s <> s_two_digit) nums
            in
            if List.length filtered_nums = List.length nums then false
            else
              check_all_nums_used_once
                (String.length str - 2 |> String.sub str 2)
                filtered_nums
          with Failure _ ->
            let filtered_nums =
              List.filter
                (fun s ->
                  s <> (s_first |> Char.escaped |> int_of_string))
                nums
            in
            if List.length filtered_nums = List.length nums then false
            else
              check_all_nums_used_once
                (String.length str - 1 |> String.sub str 1)
                filtered_nums
        else
          let filtered_nums =
            List.filter
              (fun s -> s <> (s_first |> Char.escaped |> int_of_string))
              nums
          in
          if List.length filtered_nums = List.length nums then false
          else
            check_all_nums_used_once
              (String.length str - 1 |> String.sub str 1)
              filtered_nums
    | _ ->
        check_all_nums_used_once
          (String.length str - 1 |> String.sub str 1)
          nums

let check_solution_valid (str : string) (nums : int list) : bool =
  check_valid_char str
  && check_all_nums_used_once str nums
  && check_valid_operations str 's'
  && Stack.create () |> check_valid_paren str

let pop_with_no_return paren_stack =
  Stack.pop paren_stack;
  ()

(* Number of contiguous parenthesis at the start. ex: (((5))) returns
   3. *)
let starting_paren_num (sol : string) =
  let rec num_paren (index : int) =
    if sol.[index] = '(' || sol.[index] = '[' then num_paren (index + 1)
    else index
  in
  num_paren 0

(* If the expression has surrounding parentheses that are unnecessary,
   then the surrounding parentheses are deleted (ex: (2+4) becomes
   2+4) *)
let no_initial_paren (sol : string) : string =
  let paren_stack = Stack.create () in
  let rec find_paren (index : int) (min_stack : int) =
    if index = String.length sol - min_stack then
      String.sub sol
        (Stack.length paren_stack)
        (String.length sol - (min_stack * 2))
    else
      match sol.[index] with
      | '(' | '[' ->
          Stack.push '(' paren_stack;
          find_paren (index + 1) min_stack
      | ')' | ']' ->
          pop_with_no_return paren_stack;
          find_paren (index + 1)
            (min min_stack (Stack.length paren_stack))
      | _ -> find_paren (index + 1) min_stack
  in
  find_paren 0 (starting_paren_num sol)

(* Return index of first expression. It should be + at the highest level
   (no parenthesis) but if + doesn't exist it is * For example:
   (2+3)*(5+6) returns the index of the *, while 2*3+5+6 returns the
   index of the first +. *)
let find_first_expression_num (sol : string) : int =
  let paren_stack = Stack.create () in
  let rec find_paren (index : int) (last_multi_div : int) =
    if index >= String.length sol then last_multi_div
    else
      match sol.[index] with
      | '(' | '[' ->
          Stack.push '(' paren_stack;
          find_paren (index + 1) last_multi_div
      | ')' | ']' ->
          pop_with_no_return paren_stack;
          find_paren (index + 1) last_multi_div
      | ('+' | '-') when Stack.is_empty paren_stack -> index
      | ('*' | 'x' | '/') when Stack.is_empty paren_stack ->
          find_paren (index + 1) index
      | _ -> find_paren (index + 1) last_multi_div
  in
  find_paren 0 0

(* Convert the character operators to the type operator *)
let get_operator (sol : string) (expression_index : int) =
  match sol.[expression_index] with
  | '+' -> Addition
  | '-' -> Subtraction
  | '*' | 'x' -> Multiplication
  | '/' -> Division
  | _ -> Addition

let rec expression_tree_creator (sol : string) : tree =
  let sol = no_initial_paren sol in
  match int_of_string_opt sol with
  | Some value -> Leaf value
  | None ->
      let expression_index = find_first_expression_num sol in
      Node
        ( get_operator sol expression_index,
          expression_tree_creator (String.sub sol 0 expression_index),
          expression_tree_creator
            (String.sub sol (expression_index + 1)
               (String.length sol - (expression_index + 1))) )

let rec inorder_tree = function
  | Leaf value -> [ value ]
  | Node (value, left, right) ->
      (inorder_tree left @ [ 999 ]) @ inorder_tree right

let strip_spaces (sol : string) =
  let split_sol = String.split_on_char ' ' sol in
  List.fold_left (fun acc cur -> acc ^ cur) "" split_sol

(* sol has to be >= 2 *)
let rec format_paren_multi (sol : string) =
  let rec paren_recurs (index : int) =
    if index < String.length sol then
      match (sol.[index - 1], sol.[index]) with
      | '0' .. '9', '(' | '0' .. '9', '[' ->
          (format_paren_multi (String.sub sol 0 index) ^ "*")
          ^ format_paren_multi
              (String.sub sol index (String.length sol - index))
      | _ -> paren_recurs (index + 1)
    else sol
  in
  paren_recurs 1

let check_expression_tree (t : tree) : bool =
  let rec evaluate_tree (t : tree) : int =
    match t with
    | Leaf num -> num
    | Node (op, t1, t2) -> begin
        match op with
        | Addition -> evaluate_tree t1 + evaluate_tree t2
        | Subtraction -> evaluate_tree t1 - evaluate_tree t2
        | Multiplication -> evaluate_tree t1 * evaluate_tree t2
        | Division -> evaluate_tree t / evaluate_tree t2
      end
  in
  evaluate_tree t = 24
