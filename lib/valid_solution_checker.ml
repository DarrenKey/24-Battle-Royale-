type operator =
  | Multiplication
  | Division
  | Addition
  | Subtraction

type solution_output =
  | Invalid
  | Incorrect
  | Correct

type tree =
  | Leaf of int
  | Node of operator * tree * tree

(* Makes certain that the characters in the input string only contain
   valid characters for an expression *)
let rec check_valid_char (str : string) : bool =
  if str = "" then true
  else
    let s = String.get str 0 in
    match s with
    | '0' .. '9' | '+' | '/' | '-' | 'x' | '*' | '[' | ']' | '(' | ')'
      ->
        String.length str - 1 |> String.sub str 1 |> check_valid_char
    | _ -> false

(* Checks that the parenthesis in the expression are balanced properly
   using a stack *)
let rec check_valid_paren (str : string) (paren_stack : char Stack.t) :
    bool =
  let rec paren_has_elem str =
    if String.length str < 2 then true
    else
      let s_first = String.get str 0 in
      let s_sec = String.get str 1 in
      if (s_first = '(' || s_first = '[') && (s_sec = ')' || s_sec = ']')
      then false
      else paren_has_elem (String.length str - 2 |> String.sub str 2)
  in
  if paren_has_elem str then
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
          else if Stack.pop paren_stack = '[' then
            check_valid_paren s_rest paren_stack
          else false
      | ')' ->
          if Stack.is_empty paren_stack then false
          else if Stack.pop paren_stack = '(' then
            check_valid_paren s_rest paren_stack
          else false
      | _ -> check_valid_paren s_rest paren_stack
  else false

(* Makes certain that the operations in the input string are placed in
   legal positions: between two numbers, between a number and a
   parenthesis, and between two parenthesis *)
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

(* Adds back dif_len - 1 number of elems into nums to restore the extra
   elements that were stripped when using List.filter in the
   check_all_nums_used_once function*)
let rec add_back_nums (elem : int) (nums : int list) (dif_len : int) :
    int list =
  if dif_len = 1 then nums
  else add_back_nums elem (elem :: nums) (dif_len - 1)

(* Checks if the four numbers in nums are all used exactly once in the
   input string. *)
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
              add_back_nums s_two_digit filtered_nums
                (List.length nums - List.length filtered_nums)
              |> check_all_nums_used_once
                   (String.length str - 2 |> String.sub str 2)
          with Failure _ ->
            let filtered_nums =
              List.filter
                (fun s ->
                  s <> (s_first |> Char.escaped |> int_of_string))
                nums
            in
            if List.length filtered_nums = List.length nums then false
            else
              add_back_nums
                (s_first |> Char.escaped |> int_of_string)
                filtered_nums
                (List.length nums - List.length filtered_nums)
              |> check_all_nums_used_once
                   (String.length str - 1 |> String.sub str 1)
        else
          let filtered_nums =
            List.filter
              (fun s -> s <> (s_first |> Char.escaped |> int_of_string))
              nums
          in
          if List.length filtered_nums = List.length nums then false
          else
            add_back_nums
              (s_first |> Char.escaped |> int_of_string)
              filtered_nums
              (List.length nums - List.length filtered_nums)
            |> check_all_nums_used_once
                 (String.length str - 1 |> String.sub str 1)
    | _ ->
        check_all_nums_used_once
          (String.length str - 1 |> String.sub str 1)
          nums

(* Checks if a solution string is a valid input expression by checking:
   if the characters, operations, and parenthesis are all valid and if
   all numbers in nums are used once *)
let check_solution_valid (str : string) (nums : int list) : bool =
  check_valid_char str
  && check_all_nums_used_once str nums
  && check_valid_operations str 's'
  && Stack.create () |> check_valid_paren str

(* Stack.pop function but returns a unit, exists to prevent a warning in
   later functions requiring Stack.pop *)
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
  let rec find_paren
      (index : int)
      (last_multi_div : int)
      (last_add_sub : int)
      (found_add_sub : bool) =
    if index >= String.length sol then
      if found_add_sub then last_add_sub else last_multi_div
    else
      match sol.[index] with
      | '(' | '[' ->
          Stack.push '(' paren_stack;
          find_paren (index + 1) last_multi_div last_add_sub
            found_add_sub
      | ')' | ']' ->
          pop_with_no_return paren_stack;
          find_paren (index + 1) last_multi_div last_add_sub
            found_add_sub
      | ('+' | '-') when Stack.is_empty paren_stack ->
          find_paren (index + 1) last_multi_div index true
      | ('*' | 'x' | '/') when Stack.is_empty paren_stack ->
          find_paren (index + 1) index last_add_sub found_add_sub
      | _ ->
          find_paren (index + 1) last_multi_div last_add_sub
            found_add_sub
  in
  find_paren 0 0 0 false

(* Convert the character operators to the type operator *)
let get_operator (sol : string) (expression_index : int) =
  match sol.[expression_index] with
  | '+' -> Addition
  | '-' -> Subtraction
  | '*' | 'x' -> Multiplication
  | '/' -> Division
  | _ -> Addition

(* Uses no_initial_paren as a function to remove all intial parenthesis.
   Then uses find_first_expression_num to get the index of the first
   operator. Afterwards, adds the expression to the node and recurses on
   expression left of the operator and the expression right of the
   operator.*)
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

(* strip_spaces splits sol into a list from ' ' and then concatenates
   it *)
let strip_spaces (sol : string) =
  let split_sol = String.split_on_char ' ' sol in
  List.fold_left (fun acc cur -> acc ^ cur) "" split_sol

(* sol has to be >= 2, format_paren_multi works by recursing through sol
   and finding the places where it's a number and an open parenthesis
   right after*)
let rec format_paren_multi (sol : string) =
  let rec paren_recurs (index : int) =
    if index < String.length sol then
      match (sol.[index - 1], sol.[index]) with
      | '0' .. '9', '('
      | '0' .. '9', '['
      | ')', '('
      | ']', '('
      | ')', '['
      | ']', '['
      | ')', '0' .. '9'
      | ']', '0' .. '9' ->
          (format_paren_multi (String.sub sol 0 index) ^ "*")
          ^ format_paren_multi
              (String.sub sol index (String.length sol - index))
      | _ -> paren_recurs (index + 1)
    else sol
  in
  paren_recurs 1

open Fraction

(* Checks expression tree via infix processing. Uses a fraction-library
   to prevent float rounding errors with solutions like 8/(3-8/3). *)
let check_expression_tree (t : tree) : bool =
  let rec evaluate_tree (t : tree) : frac =
    match t with
    | Leaf num -> (num, 1)
    | Node (op, t1, t2) -> begin
        match op with
        | Addition -> add_frac (evaluate_tree t1) (evaluate_tree t2)
        | Subtraction ->
            subtract_frac (evaluate_tree t1) (evaluate_tree t2)
        | Multiplication ->
            multiply_frac (evaluate_tree t1) (evaluate_tree t2)
        | Division -> divide_frac (evaluate_tree t1) (evaluate_tree t2)
      end
  in
  evaluate_tree t = (24, 1)

(* Combines all the functions above to first check if the solution is
   valid, then create an expression tree and check if it equals 24.
   Returns Invalid, Correct, and Incorrect. *)
let check_solution (sol : string) (nums : int list) : solution_output =
  let sol = sol |> strip_spaces |> format_paren_multi in
  if not (check_solution_valid sol nums) then Invalid
  else
    let expression_tree = expression_tree_creator sol in
    match check_expression_tree expression_tree with
    | true -> Correct
    | false -> Incorrect
