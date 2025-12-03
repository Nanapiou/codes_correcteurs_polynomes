open Polynomes
open Bch

module RX = MakePoly(Fields.FloatField)

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
module F16X = MakePoly(F16)

module RS15_11 = BchCode(struct
  module FqX = F16X

  let delta = 3

  open FqX
  let primitive_p = x -^ (F16.x *. one)
end)

module H7_4 = BchCode(struct (* Hamming code (7, 4) *)
  module FqX = F2X
    
  let delta = 2 
  open FqX 

  let primitive_p = x **^ 3 +^ x +^ one
end)

module Bch15 = BchCode(struct
  module FqX = F2X
  (* let m = 4 *)
  let delta = 2
  open FqX
  let primitive_p = x **^ 4 +^ x +^ one
end)

open RS15_11
open F16X

let f =
  let table = F16.of_int in
  (table 13) *. (x **^ 4) +^ (table 1) *. (x **^ 3) +^ (table 8) *. (x **^ 2) +^ (table 12) *. x +^ (table 3) *. one
  
let print_f16x p =
  let pretty_p: RX.t = Obj.magic @@ Array.map float_of_int @@ Array.map F16.to_int p in 
  print_endline (RX.to_string pretty_p)

let ef = encode f
let () =
  print_f16x ef

let e = x **^ 4

let errored = ef +^ e
let () =
  print_f16x errored
let cef = match correct errored with 
| Result.Ok p -> p 
| Result.Error _ -> failwith "Error while correcting"

let (q, r) = euclidean_div errored full_g 

let () =
  print_f16x cef;
  print_newline ();
  print_f16x q;
  print_f16x r;
  print_f16x (snd (euclidean_div e full_g));
  print_f16x (snd (euclidean_div cef full_g))
