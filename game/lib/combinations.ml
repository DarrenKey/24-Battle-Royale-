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

    let insert item index lst = if index < 0 || index > List.length lst then 
        raise (Failure "i is not in range of the list")
        else 
            let rec insert_wrapper i before n l = match i, l with
                | 0, after -> (List.rev before) @ (n::after)
                | ind, h::tail -> insert_wrapper (i-1) (h::before) n tail
                | _, after -> raise (Failure "shouldn't happen")
            in insert_wrapper index [] item lst    

  let rec permute_one num perm = match perm with
  | [] -> []
  | h::tail -> 

  let rec permute lst = match lst with
  | [] -> [[]]
  | h::tail -> 

    let rec check_ops num acc comb = match comb with
    | [] -> acc = num
    | h::tail -> check_ops num (acc +. h) tail 
      || check_ops num (acc -. h) tail
      || check_ops num (h -. acc) tail
      || check_ops num (acc *. h) tail
      || check_ops num (acc /. h) tail
      || check_ops num (h /. acc) tail

  (** [makes_24] is true iff you can get 24 with the four numbers in list [comb]
      Example: [makes_24 \[1;1;4;6\]] is true
      Requires: comb is a list of 4 positive numbers less than or equal to 12
      Raises: Invalid_argument *)
  let makes_24 comb = arr_makes_24 (Array.of_list comb)
    
end
