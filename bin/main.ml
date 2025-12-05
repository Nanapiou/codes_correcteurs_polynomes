open Algebric_structures
open Fields
open Matrixes
open Polynomes

module F2 = Fields.MakeExtendedField(struct
  module Ring = Rings.IntRing
  let p = Ring.of_int 2
end)
module F2X = MakePoly(F2)


module F16 =  Polynomes.MakePolyExtendedField(struct
  module Ring = F2X 

  open Ring
  let p = x **^ 4 +^ x +^ one
end)

let n = 3

module MnR = MakeMatrixes(struct
  module F = FloatField
  let n = n
end)

open MnR

let m = of_int_matrix [|
  [| 1; 2; 3 |];
  [| 4; 5; 6 |];
  [| 7; 8; 9 |]
|]

let () =
  row_addition m 0 (FloatField.of_int 5) 1

let () = 
  print_endline @@ to_string m
