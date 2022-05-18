(** TEST PLAN

    We used OUnit testing to test our functions that generated our
    four-number combinations. We also used it for expression related
    functions that check the validity of a user input. Specifically, we
    used OUnit to test the Combinations, Fraction, Postfix, and
    Valid_solution_checker modules. These were tested either directly or
    indirectly, using only a few overarching functions that used other
    functions.

    We tested using mainly black box and a little bit of glass box
    testing, starting with basic test cases (ex. empty list, empty
    string, basic inputs, etc) to more niche test cases (divide by zero,
    new line character, etc).

    For the multiplayer related parts of our system, we manually tested
    them. Specifically, we manually tested the Play and Timer modules.
    In the beginning, we used Postman to test our basic code using new
    libraries. When we had more functionality with our system, we
    switched to testing directly using multiple terminals with each
    terminal used to simulate the server itself as well as the different
    clients. We then moved on to using a working website to test our
    code so that we could integrate our OCaml code with the frontend
    code.

    We believe that this testing approach demostrated the correctness of
    our system. First we were able to be completely sure that our code
    relating to evaluating the possible inputs from a user worked
    perfectly. This way we could soley focus on the multiplayer aspect
    of our project. Then, as a lot of our system had to do with
    concurrency and promises, manually testing our code felt like the
    correct next step so that we could be perfectly certain of how our
    work interacted with the user. This way, we could see how
    user-friendly our system was as well as what neccessary changes we
    needed to make. *)

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

(** [in_set_test name tbl lst expected] constructs an OUnit test named
    [name] that asserts the quality of [expected] with [in_set tbl lst]. *)
let in_set_test name tbl lst expected =
  name >:: fun _ -> assert_equal expected (in_set tbl lst)

(** [insert_test name item index lst expected] constructs an OUnit test
    named [name] that asserts the quality of [expected] with
    [insert item index lst]. *)
let insert_test name item index lst expected =
  name >:: fun _ -> assert_equal expected (insert item index lst)

(** [distribute_test name item lst expected] constructs an OUnit test
    named [name] that asserts the quality of [expected] with
    [distribute item lst]. *)
let distribute_test name item lst expected =
  name >:: fun _ -> assert_equal expected (distribute item lst)

(** [makes_24_test name comb expected] constructs an OUnit test named
    [name] that asserts the quality of [expected] with [makes_24 comb]. *)
let makes_24_test name comb expected =
  name >:: fun _ -> assert_equal expected (makes_24 comb)

(** [check_sol name sol nums expected] constructs an OUnit test named
    [name] that asserts the quality of [expected] with
    [check_solution sol nums]. *)
let check_sol name sol nums expected =
  name >:: fun _ -> assert_equal expected (check_solution sol nums)

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
    makes_24_test "[2;3;5;6] makes 24" [ 2; 3; 5; 6 ] true;
  ]

let valid_sol_tests =
  [
    check_sol "6+6+6+6 makes 24" "6+6+6+6" [ 6; 6; 6; 6 ] Correct;
    check_sol "(1+1)(1+11) makes 24" "(1+1)(1+11)" [ 1; 1; 1; 11 ]
      Correct;
    check_sol "(1+1)(1+11) makes 24" "(((((1+1)))(1+11)))"
      [ 1; 1; 1; 11 ] Correct;
    check_sol "(1+1)(1+11) makes 24" "(((((1+1)))*(1+11)))"
      [ 1; 1; 1; 11 ] Correct;
    check_sol "(1+1+1)8 makes 24" "(1+1+1)8" [ 1; 1; 1; 8 ] Correct;
    check_sol "   (   1    + 1+1   )  8            makes 24"
      "   (   1    + 1+1   )  8           " [ 1; 1; 1; 8 ] Correct;
    check_sol "(1+1+1)8 makes 24" "(1+1+1)8" [ 1; 1; 1; 8 ] Correct;
    check_sol "8*((1+1)+1) makes 24" "8*((1+1)+1)" [ 1; 1; 1; 8 ]
      Correct;
    check_sol "(1+1+1)*(8) makes 24" "(1+1+1)8" [ 1; 1; 1; 8 ] Correct;
    check_sol "(1+1+1)*8 makes 24" "(1+1+1)*8" [ 1; 1; 1; 8 ] Correct;
    check_sol "(1+1+1)x8 makes 24" "(1+1+1)x8" [ 1; 1; 1; 8 ] Correct;
    check_sol "((1)+(1)+(1))x[[8]] makes 24" "((1)+(1)+(1))x[[8]]"
      [ 1; 1; 1; 8 ] Correct;
    check_sol "8/(3-8/3) makes 24" "8/(3-8/3)" [ 3; 3; 8; 8 ] Correct;
    check_sol "8/[3-8/3] makes 24" "8/[3-8/3]" [ 3; 3; 8; 8 ] Correct;
    check_sol "3*10-4-2 makes 24" "3*10-4-2" [ 3; 10; 4; 2 ] Correct;
    check_sol "(5+2-3)*6 makes 24" "(5+2-3)*6 " [ 2; 3; 5; 6 ] Correct;
    check_sol "[(5)+(2)-(3)]x6 makes 24" "[(5)+(2)-(3)]x6"
      [ 5; 2; 3; 6 ] Correct;
    check_sol "12(1+1/1) makes 24" "12(1+1/1)" [ 12; 1; 1; 1 ] Correct;
    check_sol "1*1(2)(12) makes 24" "1*1(2)(12)" [ 1; 1; 2; 12 ] Correct;
  ]

let imperfect_sol_tests =
  [
    check_sol "empty string is an invalid solution" "" [ 1; 1; 1; 8 ]
      Invalid;
    check_sol "single space is an invalid solution" " " [ 1; 1; 1; 8 ]
      Invalid;
    check_sol
      "there should not be any letters besides x for multiplication"
      "abcd" [ 1; 1; 1; 8 ] Invalid;
    check_sol "1 does not use all the numbers" "1" [ 1; 1; 1; 8 ]
      Invalid;
    check_sol "1+1+1 does not use all the numbers" "1+1+1"
      [ 1; 1; 1; 8 ] Invalid;
    check_sol "(1+1+8+1 has malformed parenthesis" "(1+1+8+1"
      [ 1; 1; 1; 8 ] Invalid;
    check_sol "(1+1+1)*8) has malformed parenthesis" "(1+1+1)*8)"
      [ 1; 1; 1; 8 ] Invalid;
    check_sol "(1*(1+1+1)*8) has too many 1s" "(1*(1+1+1)*8)"
      [ 1; 1; 1; 8 ] Invalid;
    check_sol "0+8+8+8 does not use the correct numbers" "0+8+8+8"
      [ 0; 8; 1; 8 ] Invalid;
    check_sol "8(1+1+1)a has an invalid letter in it" "8(1+1+1)a"
      [ 1; 1; 1; 8 ] Invalid;
    check_sol "8(1+1+1)+ has an invalid operation in it" "8(1+1+1)+"
      [ 1; 1; 1; 8 ] Invalid;
    check_sol "1+\n    1+1+8 is not a valid solution" "1+\n    1+1+8"
      [ 1; 1; 1; 8 ] Invalid;
    check_sol "1+1+1+8 is not a valid solution" "1+1+1+8" [ 1; 1; 1; 8 ]
      Incorrect;
    check_sol "1+7+8+9 adds to 25" "1+7+8+9" [ 1; 7; 8; 9 ] Incorrect;
    check_sol "2+8+9+4 adds to 23" "2+8+9+4" [ 2; 8; 9; 4 ] Incorrect;
    check_sol "10/(2-2)+10 does not crash" "10/(2-2)+10"
      [ 10; 2; 2; 10 ] Incorrect;
    check_sol "10--3+3+2 has an invalid operator" "10--3+3+2"
      [ 10; 3; 3; 2 ] Invalid;
    check_sol "Tab is not a valid input" "\t" [ 1; 2; 3; 4 ] Invalid;
    check_sol "Mismatched braces/brackets/parentheses" "[9+([1+1)+1]"
      [ 9; 1; 1; 1 ] Invalid;
    check_sol "Intermingled braces/brackets/parentheses" "[9+(1+1]+1)"
      [ 9; 1; 1; 1 ] Invalid;
    check_sol "Invalid parenthesis placement" "9+(1+1+)1" [ 9; 1; 1; 1 ]
      Invalid;
    check_sol "Division by 0" "9/0" [ 9; 1; 1; 1 ] Invalid;
    check_sol "9+1+1 1 is missing operators" "9+1+1 1" [ 9; 1; 1; 1 ]
      Invalid;
  ]

let suite =
  "test suite for game"
  >::: List.flatten [ comb_test; valid_sol_tests; imperfect_sol_tests ]

let _ = run_test_tt_main suite