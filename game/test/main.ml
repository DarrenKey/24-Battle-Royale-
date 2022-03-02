open OUnit2
open Game

let valid_solution_tests = []
let suite = "test suite for 24 battle royale" >::: List.flatten []
let _ = run_test_tt_main suite