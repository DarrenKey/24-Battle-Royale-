open OUnit2
open Game
open Combinations.Combinations
open Valid_solution_checker

let valid_solution_tests = []

(** [hashset] is the hash set of [lst] implemented with a Hashtbl
    Example: [hashset \[1;2;3;4\]] is {1,2,3,4} *)
let hashset lst = let empty_set = Hashtbl.create 10 in 
  let rec add_to_tbl tbl processing_list = match processing_list with 
    | [] -> tbl
    | h::tail -> Hashtbl.add tbl h false; add_to_tbl tbl tail
  in add_to_tbl empty_set lst

let in_set_test name tbl lst expected = 
  name >:: fun _ -> (assert_equal expected (in_set tbl lst));;

let comb_test = [
  in_set_test "[1;2;3;4] is not in the empty set" (hashset []) [1;2;3;4] false;
  in_set_test "[1;2;3;4] is in the set {(1,2,3,4)}" (hashset [(1,2,3,4)]) [1;2;3;4] true;
  in_set_test "[14;13;20;10] is not in the set {(1,2,3,4),(5,6,7,8)}" (hashset [(1,2,3,4);(5,6,7,8)]) [14;13;20;10] false;
  in_set_test "[4;2;3;1] is in the set {(1,2,3,4),(5,6,7,8)}" (hashset [(1,2,3,4); (5,6,7,8)]) [4;2;3;1] true;
]

let suite = "test suite for 24 battle royale" >::: List.flatten [comb_test]
let _ = run_test_tt_main suite