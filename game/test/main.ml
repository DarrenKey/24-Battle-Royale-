open OUnit2
open Game
open Combinations.CombinationsImpl
open Valid_solution_checker

(** [hashset] is the hash set of [lst] implemented with a Hashtbl
    Example: [hashset \[1;2;3;4\]] is {1,2,3,4} *)
let hashset lst =
  let empty_set = Hashtbl.create 10 in
  let rec add_to_tbl tbl processing_list =
    match processing_list with
    | [] -> tbl
    | h :: tail ->
        Hashtbl.add tbl h false;
        add_to_tbl tbl tail
  in
  add_to_tbl empty_set lst

let in_set_test name tbl lst expected =
  name >:: fun _ -> assert_equal expected (in_set tbl lst)

let insert_test name item index lst expected =
  name >:: fun _ -> assert_equal expected (insert item index lst)

let distribute_test name item lst expected =
  name >:: fun _ -> assert_equal expected (distribute item lst)

let makes_24_test name comb expected =
  name >:: fun _ -> assert_equal expected (makes_24 comb)

let comb_test =
  [
    in_set_test "[1;2;3;4] is not in the empty set" (hashset [])
      [ 1; 2; 3; 4 ] false;
    in_set_test "[1;2;3;4] is in the set {(1,2,3,4)}"
      (hashset [ (1, 2, 3, 4) ])
      [ 1; 2; 3; 4 ] true;
    in_set_test "[14;13;20;10] is not in the set {(1,2,3,4),(5,6,7,8)}"
      (hashset [ (1, 2, 3, 4); (5, 6, 7, 8) ])
      [ 14; 13; 20; 10 ] false;
    in_set_test "[4;2;3;1] is in the set {(1,2,3,4),(5,6,7,8)}"
      (hashset [ (1, 2, 3, 4); (5, 6, 7, 8) ])
      [ 4; 2; 3; 1 ] true;
    insert_test "insert 10 into [1;2;3;4] at index 0" 10 0
      [ 1; 2; 3; 4 ] [ 10; 1; 2; 3; 4 ];
    insert_test "insert 10 into [1;2;3;4] at index 1" 10 1
      [ 1; 2; 3; 4 ] [ 1; 10; 2; 3; 4 ];
    insert_test "insert 10 into [1;2;3;4] at index 2" 10 2
      [ 1; 2; 3; 4 ] [ 1; 2; 10; 3; 4 ];
    insert_test "insert 10 into [1;2;3;4] at index 4" 10 4
      [ 1; 2; 3; 4 ] [ 1; 2; 3; 4; 10 ];
    insert_test "insert 10 into [] at index 0" 10 0 [] [ 10 ];
    distribute_test "distribute 4 into [1;2;3]" 4 [ 1; 2; 3 ]
      [ [ 4; 1; 2; 3 ]; [ 1; 4; 2; 3 ]; [ 1; 2; 4; 3 ]; [ 1; 2; 3; 4 ] ];
    distribute_test "distribute 4 into [1]" 4 [ 1 ]
      [ [ 4; 1 ]; [ 1; 4 ] ];
    distribute_test "distribute 4 into []" 4 [] [ [ 4 ] ];
    makes_24_test "[3;3;8;8] makes 24" [ 3; 3; 8; 8 ] true;
    makes_24_test "[1;1;1;1] does not make 24" [ 1; 1; 1; 1 ] false;
    makes_24_test "[1;2;3;4] makes 24" [ 1; 2; 3; 4 ] true;
    makes_24_test "[4;3;2;3] does not make 24" [ 4; 3; 2; 3 ] false;
    makes_24_test "[2;3;5;12] makes 24" [ 12; 5; 2; 3 ] true;
  ]

let check_sol name sol nums expected =
  name >:: fun _ -> assert_equal expected (check_solution sol nums)

let valid_sol_tests =
  [
    check_sol "(1+1)(1+11) makes 24" "(1+1)(1+11)" [ 1; 1; 1; 11 ]
      Correct;
    check_sol "(1+1+1)8 makes 24" "(1+1+1)8" [ 1; 1; 1; 8 ] Correct;
    check_sol "(1+1+1)(8) makes 24" "(1+1+1)8" [ 1; 1; 1; 8 ] Correct;
    check_sol "(1+1+1)*(8) makes 24" "(1+1+1)8" [ 1; 1; 1; 8 ] Correct;
    check_sol "(1+1+1)*8 makes 24" "(1+1+1)8" [ 1; 1; 1; 8 ] Correct;
    check_sol "(1+1+1)*8) invalid input" "(1+1+1)8)" [ 1; 1; 1; 8 ]
      Invalid;
    check_sol "8/(3-8/3) makes 24" "8/(3-8/3)" [ 3; 3; 8; 8 ] Correct;
    check_sol "3*10-4-2 makes 24" "3*10-4-2" [ 3; 10; 4; 2 ] Correct;
  ]

let suite =
  "test suite for game" >::: List.flatten [ comb_test; valid_sol_tests ]

let _ = run_test_tt_main suite