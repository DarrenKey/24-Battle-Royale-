(** Module to check if a submitted solution is valid or not, and if the
    provided operations equals 24.

    The solution, given as a string, is converted into a binary
    expression tree.

    Precondition : solution given as a string without any spaces.*)

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

(** Implementation of the binary expression tree. *)

(** -------------Helper methods for [check_solution_valid]------------*)

val check_valid_char : string -> bool
(** [check_valid_char s] checks if the solution [s] contains only valid
    characters. Valid characters are: -All numbers 0 - 9 -Operators +,
    /, -, x or * -Parenthesis/Brackets [], () *)

val check_valid_paren : string -> char Stack.t -> bool
(** [check_valid_paren str paren_stack] checks if the solution [s] has
    valid matching parenthesis using [paren_stack]. Legal parenthesis
    are [] and (), but not {}. *)

val check_valid_operations : string -> char -> bool
(** [check_valid_operations s] checks if the solution [s] has valid
    operations order. Legal operations are + for addition, / for
    division, - for subtraction, x or * for multipication. *)

val add_back_nums : int -> int list -> int -> int list

val check_all_nums_used_once : string -> int list -> bool
(** [check_all_nums_used_once s lst] checks if the solution [s] has all
    of the given numbers in [lst] are used exactly once.

    Precondition - [lst] contains positive numbers with one digit or two
    digits. *)

(** ------------------ Overview methods
    \---------------------------------*)
val check_solution_valid : string -> int list -> bool
(** [check_solution_valid s] uses all the helper methods above to check
    if the solution [s] is a valid solution.

    Precondition - [s] is a string. Postcondition - returns a string
    which can be parsed into a binary expression tree where every number
    is used once and only once. *)

val expression_tree_creator : string -> tree
(** [expression_tree_creator s] creates an expression tree from the
    submitted solution [s].

    Precondition - [s] is a valid string which can be parsed into a
    binary expression tree and has every combination of numbers used
    once and only once .

    Postcondition - returns a valid binary expression tree. *)

val no_initial_paren : string -> string
val strip_spaces : string -> string
val format_paren_multi : string -> string

val check_expression_tree : tree -> bool
(** [check_expression_tree t] checks whether or not the tree [t]
    evaluates to 24.*)

val check_solution : string -> int list -> solution_output
(** [check_solution s] combines the functions above and returns whether
    or not the solution [s] is invalid (inputted wrong), incorrect (does
    not equal 24), or correct (does equal 24). *)
