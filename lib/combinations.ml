module type Combinations = sig
  val add_to_set :
    ('a * 'a * 'a * 'a, bool) Hashtbl.t -> 'a list -> unit
  (** [add_to_set] is the set with [lst] as sorted tuple added to tbl *)

  val in_set : ('a * 'a * 'a * 'a, bool) Hashtbl.t -> 'a list -> bool
  (** [in_set] is whether or not the combination of 4 numbers
      represented by [lst] is in [tbl]. Example:
      [in_set {(1,2,3,4)} \[2;3;1;4\]] Requires: lst is a list of 4
      numbers. Raises: Invalid_argument *)

  val insert : 'a -> int -> 'a list -> 'a list
  (** [insert]s item into [lst] at [index]. [index] is where [item]
      would be after the insertion *)

  val makes_24 : int list -> bool
  (** [makes_24] is true iff you can get 24 with the four numbers in
      list [comb] Example: [makes_24 \[1;1;4;6\]] is true. Requires:
      comb is a list of 4 positive numbers less than or equal to 12.
      Raises: Invalid_argument *)

  (** Generates the solution to [lst] in Postfix. Requires: [lst] can
      get to 24 *)
end

module CombinationsImpl = struct
  let list_to_tuple lst =
    match lst with
    | [ a; b; c; d ] -> (a, b, c, d)
    | _ -> raise (Invalid_argument "[lst] is not a list of 4 numbers")

  let add_to_set tbl lst =
    match List.sort compare lst with
    | [ a; b; c; d ] -> Hashtbl.add tbl (a, b, c, d) false
    | _ -> raise (Invalid_argument "[lst] is not a list of 4 numbers")

  let in_set tbl lst =
    match lst with
    | [ _; _; _; _ ] ->
        List.sort compare lst |> list_to_tuple |> Hashtbl.mem tbl
    | _ -> raise (Invalid_argument "[lst] is not a list of 4 numbers")

  let insert item index lst =
    if index < 0 || index > List.length lst then
      raise (Failure "i is not in range of the list")
    else
      let rec insert_wrapper i before n l =
        match (i, l) with
        | 0, after -> List.rev before @ (n :: after)
        | ind, h :: tail -> insert_wrapper (i - 1) (h :: before) n tail
        | _, after -> raise (Failure "shouldn't happen")
      in
      insert_wrapper index [] item lst

  let distribute item lst =
    let rec distribute_wrapper n l i acc =
      match i with
      | -1 -> acc
      | ind -> distribute_wrapper n l (i - 1) (insert n ind l :: acc)
    in
    distribute_wrapper item lst (List.length lst) []

  let rec permute_one num lst =
    match lst with
    | [] -> []
    | h :: tail -> distribute num h @ permute_one num tail

  let rec permute lst =
    match lst with
    | [] -> [ [] ]
    | h :: tail -> permute_one h (permute tail)

  (** Construct all possible orderings of [lst] *)
  let rec nested_fors lst i j k =
    match (i, j, k) with
    | -1, j, k -> []
    | i, j, -1 -> nested_fors lst i (j - 1) (List.length lst - 1)
    | i, -1, k ->
        nested_fors lst (i - 1)
          (List.length lst - 1)
          (List.length lst - 1)
    | i, j, k ->
        [
          Postfix.Operator (List.nth lst i);
          Postfix.Operator (List.nth lst j);
          Postfix.Operator (List.nth lst k);
        ]
        :: nested_fors lst i j (k - 1)

  (** parses operators for fractions from the rules *)
  let operations = Postfix.ops_of_rules Postfix.frac_rules

  (** Constructs at least all possible combination of [ops] *)
  let ops_combos ops : Fraction.frac Postfix.token list list =
    nested_fors ops
      (List.length ops - 1)
      (List.length ops - 1)
      (List.length ops - 1)
    |> List.map (List.sort compare)
    |> List.sort_uniq compare

  let cons_postfix comb ops =
    List.map
      (( @ ) [ Postfix.Operand (List.hd comb) ])
      (permute (Postfix.to_tokens (List.tl comb) @ ops))

  (** Constructs every postfix expression possible with the numbers in
      [comb] and the operators in [ops], then checks them to see if one
      of them evaluates to [target] *)
  let cons_postfix_and_check target comb ops =
    let rec check_exprs e =
      match e with
      | [] -> false
      | expr :: tail ->
          (try Postfix.eval_postfix Postfix.frac_rules expr = (target, 1)
           with Failure _ -> false)
          || check_exprs tail
    in
    check_exprs (cons_postfix comb ops)

  (** checks if this combination of operators [op_combos] can make
      [target] with the numbers in [comb]*)
  let rec check_op_combo_valid target comb op_combos =
    match op_combos with
    | [] -> false
    | op_combo :: tail ->
        cons_postfix_and_check target comb op_combo
        || check_op_combo_valid target comb tail

  (** converts 'a list to Fraction.frac list*)
  let rec comb_to_frac_list comb =
    match comb with
    | [] -> []
    | h :: tail -> (h, 1) :: comb_to_frac_list tail

  (** checks if you can make [target] with [comb] *)
  let check_ops target comb =
    check_op_combo_valid target
      (comb_to_frac_list comb)
      (ops_combos operations)

  let makes_target target comb =
    let rec makes_target_wrapper target permutations =
      match permutations with
      | [] -> false
      | h :: tail ->
          check_ops target h || makes_target_wrapper target tail
    in
    makes_target_wrapper target (permute comb)

  let makes_24 comb = makes_target 24 comb

  (** [None] if there are no solutions, [Some expr] if expr evaluates to
      24 *)
  let get_valid_postfix target comb ops =
    let rec check_exprs e =
      match e with
      | [] -> None
      | expr :: tail ->
          if
            try
              Postfix.eval_postfix Postfix.frac_rules expr = (target, 1)
            with Failure _ -> false
          then Some expr
          else check_exprs tail
    in
    check_exprs (cons_postfix comb ops)

  let get_valid_ops t c =
    let rec get_valid_ops_wrapper target comb op_combos =
      match op_combos with
      | [] -> None
      | op_combo :: tail -> (
          match get_valid_postfix target comb op_combo with
          | None -> get_valid_ops_wrapper target comb tail
          | Some expr -> Some expr)
    in
    get_valid_ops_wrapper t (comb_to_frac_list c)
      (ops_combos operations)

  let get_target target comb =
    let rec get_target_wrapper target permutations =
      match permutations with
      | [] -> raise Not_found
      | h :: tail -> (
          match get_valid_ops target h with
          | None -> get_target_wrapper target tail
          | Some expr -> expr)
    in
    get_target_wrapper target (permute comb)

  let postfix_solution lst =
    get_target 24 lst
    |> List.map (fun x ->
           match x with
           | Postfix.Operand (num, _) -> Postfix.Operand num
           | Postfix.Operator x -> Postfix.Operator x)
    |> Postfix.postfix_to_infix
end

module Combinations : Combinations = CombinationsImpl