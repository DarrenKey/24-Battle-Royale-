type operator =
  | Multiplication
  | Division
  | Addition
  | Subtraction

type tree =
  | Leaf of int
  | Node of operator * tree * tree

let check_valid_char (_ : string) =
  raise (Failure "Unimplemented: Main.play_game")

let check_valid_paren (_ : string) =
  raise (Failure "Unimplemented: Main.play_game")

let check_valid_operations (_ : string) =
  raise (Failure "Unimplemented: Main.play_game")

let check_all_nums_used_once (_ : string) (_ : int * int * int * int) =
  raise (Failure "Unimplemented: Main.play_game")

let check_solution_valid (_ : string) (_ : int * int * int * int) =
  raise (Failure "Unimplemented: Main.play_game")

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

let check_expression_tree (_ : tree) =
  raise (Failure "Unimplemented: Main.play_game")
