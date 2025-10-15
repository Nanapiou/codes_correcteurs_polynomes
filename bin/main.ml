open Polynomes
open Bch

module F2 = Fields.MakeExtendedField(struct
  module Ring = Rings.IntRing
  let p = Ring.of_int 2
end)
module F2X = MakePoly(F2)
module F16 = MakePolyExtendedField(struct
  module Ring = F2X
  open Ring  

  let p = x **^ 4 +^ x +^ one
end)

open F16

let () =
  print_endline @@ to_string (x *^ x);
  print_endline @@ to_string ((F16.inv (one +^ x)) *^ (one +^ x **^ 2))

module Bch15 = BchCode(struct
  module PF = F2X
  let m = 4
  let delta = 5

  open PF
  let primitive_p = x **^ 4 +^ x +^ one
end)

open Bch15

let sigma7 = sigma 7

let () = print_int_set sigma7

let print_int_array a =
  print_char '[';
  Array.iter (Printf.printf "%d, ") a;
  print_endline "]"

let ( *^ ) = F2X.( *^ )
let () =
  print_endline @@ F2X.to_string (g 3);
  print_endline @@ F2X.to_string (g 5);
  print_endline @@ F2X.to_string ((g 1) *^ (g 3) *^ (g 5))

let gt7 = gtemp 7
let () = print_int_array gt7
  
