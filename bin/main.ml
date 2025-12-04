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

module MnF16 = MakeMatrixes(struct
  module F = F16
  let n = n
end)

let m = MnF16.of_int_matrix [|
  [| 1; 2; 3 |];
  [| 4; 5; 6 |];
  [| 7; 8; 9 |]
|]

let () = 
  print_endline @@ MnF16.to_string m
