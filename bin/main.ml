open Polynomes

module F2 = Fields.MakeExtendedField(struct
  module Ring = Rings.IntRing
  let p = Ring.of_int 2
end)
module F2X = MakePoly(F2)
module F16 = Fields.MakeExtendedField(struct
  module Ring = F2X
  let ( **^ ) = F2X.exp
  let ( +^ ) = F2X.add
  let x = F2X.of_int 1

  let p = x **^ 4 +^ x +^ F2X.one
end)

module RX = MakePoly(Fields.FloatField)
module C = Fields.MakeExtendedField(struct
  module Ring = RX 
  let ( **^ ) = RX.exp
  let ( +^ ) = RX.add
  let x = RX.of_int 1

  let p = x **^ 2 +^ RX.one
end)

let one = C.one
let i = C.of_int 1 

let () =
  print_string @@ C.to_string one;
  print_newline ();
  print_string @@ C.to_string (C.mul i i);
  print_newline ()
