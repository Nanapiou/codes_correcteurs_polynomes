open Polynomes
open Bch

module F2 = Fields.MakeExtendedField(struct
  module Ring = Rings.IntRing
  let p = Ring.of_int 2
end)
module F2X = MakePoly(F2)
(* module F16 = MakePolyExtendedField(struct *)
  (* module Ring = F2X *)
  (* open Ring *)  

  (* let p = x **^ 4 +^ x +^ one *)
(* end) *)

module Bch15 = BchCode(struct
  module PF = F2X
  let m = 4
  let delta = 5

  open PF
  let primitive_p = x **^ 4 +^ x +^ one
end)

open Bch15

let _print_int_array a =
  print_char '[';
  Array.iter (Printf.printf "%d, ") a;
  print_endline "]"

let () = print_int_set @@ full_sigma

let ( *^ ) = F2X.( *^ )
let () =
  print_endline @@ F2X.to_string full_g;
  print_endline @@ F2X.to_string ((g 1) *^ (g 3))
  
