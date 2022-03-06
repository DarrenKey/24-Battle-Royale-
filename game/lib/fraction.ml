type frac = int * int

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let reduce_frac (n : frac) =
  match n with
  | a, b ->
      let gcd_frac = gcd a b in
      (a / gcd_frac, b / gcd_frac)

let add_frac (a : frac) (b : frac) =
  match (a, b) with
  | (a1, a2), (b1, b2) -> reduce_frac ((a1 * b2) + (b1 * a2), a2 * b2)

let subtract_frac (a : frac) (b : frac) =
  match (a, b) with
  | (a1, a2), (b1, b2) -> reduce_frac ((a1 * b2) - (b1 * a2), a2 * b2)

let multiply_frac (a : frac) (b : frac) =
  match (a, b) with
  | (a1, a2), (b1, b2) -> reduce_frac (a1 * b1, a2 * b2)

let divide_frac (a : frac) (b : frac) =
  match (a, b) with
  | (a1, a2), (b1, b2) -> reduce_frac (a1 * b2, a2 * b1)
