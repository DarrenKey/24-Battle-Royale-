(** Module to implement fractions to avoid floating error rounding. Some
    solutions are solvable only by dividing into fractions; take
    3,3,8,8, which only has 8/(3-8/3) as a solution. *)

type frac = int * int
(** frac [(a b)] represents a fraction with the numerator a and the 
    denominator b.*)

val gcd : int -> int -> int
(** [gcd a b] finds the greatest common divisor between [a] and [b]. *)

val reduce_frac : frac -> frac
(** [reduce_frac n] reduces the fraction [n]. For instance, (2,4)
    returns (1,2). *)

val add_frac : frac -> frac -> frac
(** [add_frac a b] adds the fractions [a] and [b] and returns the
    simplified fraction (for instance, (1,4) + (1,4) returns (1,2)
    instead of (2,4)). *)

val subtract_frac : frac -> frac -> frac
(** [subtract_frac a b] subtracts the fraction [a] from the fraction [b]
    and returns the simplified fraction.*)

val multiply_frac : frac -> frac -> frac
(** [multiply_frac a b] multiplies the fractions [a] and [b] and returns
    the simplified fraction. *)

val divide_frac : frac -> frac -> frac
(** [divide_frac a b] divides the fraction [a] from the fraction [b] and
    returns the simplified fraction.*)