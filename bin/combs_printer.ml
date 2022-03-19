open Game.Combinations.Combinations

(* Doesn't work, when you try to apply this function to a string it
   demands a format type

   let append_to_combos str = let append_printer file str = let o =
   open_out_gen [Open_append; Open_creat] 0o666 file in Printf.fprintf o
   str; close_out o in append_printer "./combos.txt" str;; *)

(* This function is contingent upon append_to_combos being a higher
   order function

   let format_comb a b c d = ((string_of_int a)^" "^(string_of_int b)^"
   "^(string_of_int c)^" "^(string_of_int d)^"\n") *)

(* 15 choose 11 (balls and bins with 12 bins and 4 balls) *)
let combs = 1365

(** [seen_combs] is the set containing all combinations that have been
    generated*)
let seen_combs : (int * int * int * int, bool) Hashtbl.t =
  Hashtbl.create combs

let print_valid_combs output_file =
  for a = 1 to 13 do
    for b = 1 to 13 do
      for c = 1 to 13 do
        for d = 1 to 13 do
          if
            [ a; b; c; d ] |> in_set seen_combs = false
            && makes_24 [ a; b; c; d ]
          then (
            let o =
              open_out_gen
                [ Open_append; Open_creat ]
                0o666
                ("./assets/" ^ output_file)
            in
            Printf.fprintf o "%d %d %d %d\n" a b c d;
            close_out o);
          add_to_set seen_combs [ a; b; c; d ]
        done
      done
    done
  done
;;

print_valid_combs "combos.txt"
