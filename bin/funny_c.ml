open Polynomes

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
