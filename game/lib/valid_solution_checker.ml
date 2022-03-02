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

let check_all_nums_used_once
    (_ : string)
    (_ : int * int * int * int) =
  raise (Failure "Unimplemented: Main.play_game")

let check_solution_valid
(_ : string)
(_ : int * int * int * int) =
raise (Failure "Unimplemented: Main.play_game")

let expression_tree_creator (_ : string) =
  raise (Failure "Unimplemented: Main.play_game")

let check_expression_tree (_ : tree) =
  raise (Failure "Unimplemented: Main.play_game")
