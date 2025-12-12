open Algebric_structures
open Fields
open Matrixes
open Polynomes

module F3 = Fields.MakeExtendedField(struct
  module Ring = Rings.IntRing
  let p = Ring.of_int 3
end)
module F3X = MakePoly(F3)

let p: F3X.t = 
  let open F3X in
  ((x **^ 3) +^ x +^ one) *^ (x +^ one) *^ x
  (* x **^ 2 +^ x *)

let (coef, factors) = F3X.berlekamp_irreductible p
let pback = List.fold_left F3X.mul F3X.one factors

let () =
  (* print_endline @@ F3X.to_string p; *)
  print_endline @@ F3X.to_string p;
  List.iter (Fun.compose print_endline F3X.to_string) @@ factors
