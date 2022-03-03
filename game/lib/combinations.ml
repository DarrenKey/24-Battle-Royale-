module type Combinations = sig

end

module Combinations = struct

  (** [seen_combs] is the set containing all combinations that have been generated*)
  let seen_combs num_combs = Hashtbl.create num_combs

  (** [list_to_tuple] converts [lst] of 4 numbers to tuple
      Example: [list_to_tuple \[1;2;3;4\]] is [(1,2,3,4)]
      Requires: lst is a list of 4 elements
      Raises: Invalid_argument *)
  let list_to_tuple lst = match lst with
  | a::b::c::d::[] -> (a, b, c, d)
  | _ -> raise (Invalid_argument "[lst] is not a list of 4 numbers")

  (** [in_set] is whether or not the combination of 4 numbers represented by [lst] is in [tbl] 
      Example: [in_set {(1,2,3,4)} \[2;3;1;4\]]
      Requires: lst is a list of 4 numbers
      Raises: Invalid_argument *)
  let in_set tbl lst = match lst with
  | _::_::_::_::[] -> List.sort compare lst |> list_to_tuple |> Hashtbl.mem tbl 
  | _ -> raise (Invalid_argument "[lst] is not a list of 4 numbers")
  
  (** [is_valid_comb] is true iff you can get 24 with the four numbers in list [comb]
      Example: [is_valid_comb \[1;1;4;6\]] is true
      Requires: comb is a list of 4 positive numbers less than or equal to 12
      Raises: Invalid_argument *)
  let is_valid_comb comb = failwith "not implemented"

end
