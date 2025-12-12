(* open Algebric_structures
open Fields
open Matrixes
open Polynomes

module F2 = Fields.MakeExtendedField(struct
  module Ring = Rings.IntRing
  let p = Ring.of_int 2
end)
module F2X = MakePoly(F2)

let p: F2X.t = 
  let open F2X in
  cyclotomic 7

let (coef, factors) = F2X.berlekamp_irreductible p
let pback = List.fold_left F2X.mul F2X.one factors

let () =
  print_endline @@ F2X.to_string p;
  List.iter (Fun.compose print_endline F2X.to_string) @@ factors *)
