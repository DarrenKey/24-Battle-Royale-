(** Module to check if a submitted solution is valid or not, 
    and if the provided operations equals 24.  
        
    The solution, given as a string, is converted into a binary
    expression tree. *)

type tree
(** Implementation of the binary expression tree. *)



(** -------------Helper methods for [check_solution_valid]------------*)


val check_valid_char : string -> bool
(** [check_valid_char s] checks if the solution [s] contains only valid 
    characters. Valid characters are:
    -All numbers 0 - 9
    -Operators +, -/, x or * 
    -Parenthesis/Brackets [], () *)

val check_valid_paren : string -> bool
(** [check_valid_paren s] checks if the solution [s] has valid matching
    parenthesis. *)

val check_valid_operations : string -> bool
(** [check_valid_operations s] checks if the solution [s] has valid operations
    order. *)

val check_all_nums_used_once : string -> int * int * int * int -> bool
(** [check_all_nums_used_once s] checks if the solution [s] has all of the given
    numbers are used once. *)

(** ------------------ Overview methods ---------------------------------*)
val check_solution_valid : string -> int * int * int * int -> bool
(** [check_solution_valid s] uses all the helper methods above to check
    if the solution [s] is a valid solution.
        
    Precondition - [s] is a string.
    Postcondition - returns a string which can be parsed into a
    binary expression tree where every number is used once and only once. *)

val expression_tree_creator : string -> tree
(** [expression_tree_creator s] creates an expression tree 
    from the submitted solution [s].
    
    Precondition - [s] is a string which can be parsed into a binary
    expression tree.
    
    Postcondition - returns a valid binary expression tree. *)

val check_expression_tree : tree -> bool
(** [check_expression_tree t] checks whether or not the tree [t]
    equals 24.*)