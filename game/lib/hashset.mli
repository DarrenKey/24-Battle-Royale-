type 'a t

val create : int -> 'a t

val add : 'a t -> 'a -> unit
(** [add_to_set] is the set with [lst] as sorted tuple added to tbl *)

val mem : 'a t -> 'a -> bool
(** [mem] is whether or not the combination of 4 numbers represented by
    [lst] is in [tbl]. Example: [in_set {\[1,2,3,4\]} \[2;3;1;4\]]
    evaluates to true. Requires: lst is a list of 4 numbers. Raises:
    Invalid_argument *)
