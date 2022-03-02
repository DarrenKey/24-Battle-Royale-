module type Combinations = sig

end

module Combinations = struct

  (** [seen_combs] is the set containing all combinations that have been generated*)
  let seen_combs num_combs = Hashtbl.create num_combs

  (** [list_to_tuple] converts [lst] of 4 numbers to tuple*)
  let list_to_tuple lst = match lst with
  | a::b::c::d::[] -> (a, b, c, d)
  | _ -> raise (Invalid_argument "[lst] is not a list of 4 numbers")

  (** [in_set] is whether or not [lst] of 4 numbers is in [tbl]*)
  let in_set tbl lst = match lst with
  | _::_::_::_::[] -> List.sort compare lst |> list_to_tuple |> Hashtbl.mem tbl 
  | _ -> raise (Invalid_argument "[lst] is not a list of 4 numbers")
  
  (** [isvalid_comb] is true iff you can get 24 with the four numbers in comb*)
  let is_valid_comb comb = failwith "not implemented"

end
