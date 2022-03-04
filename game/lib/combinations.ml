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
  
  let input_is_valid lst = match lst with 
  | _::_::_::_::[] -> true
  | _ -> false

  (** [insert]s item into [lst] at [index]. [index] is where [item] would be after the insertion *)
  let insert item index lst = 
    if index < 0 || index > List.length lst then 
      raise (Failure "i is not in range of the list")
    else 
      let rec insert_wrapper i before n l = match i, l with
      | 0, after -> (List.rev before) @ (n::after)
      | ind, h::tail -> insert_wrapper (i-1) (h::before) n tail
      | _, after -> raise (Failure "shouldn't happen")
      in insert_wrapper index [] item lst    

  let distribute item lst = 
    let rec distribute_wrapper n l i acc = match i with 
    | -1 -> acc
    | ind -> distribute_wrapper n l (i-1) ((insert n ind l)::acc)
  in distribute_wrapper item lst (List.length lst) []

  let rec permute_one num lst = match lst with
  | [] -> []
  | h::tail -> (distribute num h)@(permute_one num tail)

  let rec permute lst = match lst with
  | [] -> [[]]
  | h::tail -> (permute_one h (permute tail))

  let rec check_ops_wrapper num acc comb = match comb with
  (* TODO: fix the floating point approximation bug!! *)
  | [] -> (*(if 23.99 < acc && acc < 24.01 then print_endline (string_of_float acc));*) acc = num
  | h::tail -> check_ops_wrapper num (acc +. h) tail 
    || check_ops_wrapper num (acc -. h) tail
    || check_ops_wrapper num (h -. acc) tail
    || check_ops_wrapper num (acc *. h) tail
    || check_ops_wrapper num (acc /. h) tail
    || check_ops_wrapper num (h /. acc) tail

  let check_ops num comb = match List.map float_of_int comb with
  | h::tail -> check_ops_wrapper (float_of_int num) h tail
  | _ -> raise (Invalid_argument "comb must be a list of 4 elements")

  (** [makes_24] is true iff you can get 24 with the four numbers in list [comb]
      Example: [makes_24 \[1;1;4;6\]] is true
      Requires: comb is a list of 4 positive numbers less than or equal to 12
      Raises: Invalid_argument *)
  let makes_24 comb = 
    let rec makes_24_wrapper permutations = match permutations with
    | [] -> false
    | h::tail -> (check_ops 24 h) || makes_24_wrapper tail
  in makes_24_wrapper (permute comb)
    
end
