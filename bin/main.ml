open Polynomes

module F2 = Fields.MakeExtendedField(struct
  module Ring = Rings.IntRing
  let p = Ring.of_int 2
end)
module F2X = MakePoly(F2)
module F16 = MakePolyExtendedField(struct
  module Ring = F2X
  open Ring  

  let p = x **^ 4 +^ x +^ F2X.one
end)

module RX = MakePoly(Fields.FloatField)
module C = MakePolyExtendedField(struct
  module Ring = RX 
  open Ring

  let p = x **^ 2 +^ RX.one
end)


let one = C.one
let i = C.of_int 1

let () =
  print_string @@ C.to_string (C.mul i i);
  print_newline ();
  print_string @@ C.to_string (C.inv (C.sub one i));
  print_newline ()
