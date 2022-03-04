open OUnit2
open Game
open Combinations.Combinations
open Valid_solution_checker

let valid_solution_tests = []

(* let ht = Hashtbl.create 10 *)
let in_set_test name tbl lst expected =
  name >:: fun _ -> assert_equal expected (in_set tbl lst)

let valid_sol_test name sol expected =
  name >:: fun _ -> assert_equal expected (expression_tree_creator sol)

let comb_test =
  [ (* in_set_test "[1,2,3,4] is not in the empty set" ht [1;2;3;4]
       false *) ]

let valid_sol_test = []

let suite =
  "test suite for 24 battle royale" >::: List.flatten [ comb_test ]

let _ = run_test_tt_main suite