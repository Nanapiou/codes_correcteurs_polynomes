open Polynomes

module IntSet = Set.Make(Int)
let print_int_set s =
  print_char '{';
  IntSet.iter (Printf.printf "%d, ") s;
  print_endline "}"

module type BCH_PARAM = sig
  module PF : POLY_EUCLIDEAN_RING 
  val primitive_p: PF.t (* An irreductible factor of phi_{q^m-1} *)

  val m : int
  val delta : int
end

module BchCode(P: BCH_PARAM) = struct
  module PF = P.PF
  module F = PF.F
  let delta = if P.delta mod 2 = 0 then P.delta + 1 else P.delta (* Gives the same sigma *)
  let q = F.order
  let () = if q = -1 then failwith "Do not use a BCH on a non-finite field."
  let m = P.m
  let t = delta / 2

  let n =
    let open Rings.IntRing in
    to_int (exp (of_int q) m) - 1

  module Fqm = MakePolyExtendedField(struct
    module Ring = PF
    let p = P.primitive_p
  end)
  module FqmX = MakePoly(Fqm) 
  (* let alpha = FqmX.F.of_int 1 (* Just Fqm.x, but now it works with types... *) *)
  let alpha = Fqm.x
  (* Alpha is now a root of primitive_p *)
  
  module ZnZ = Fields.MakeExtendedField(struct
    module Ring = Rings.IntRing
    let p = Ring.of_int n
  end)

  let sigma i =
    let i = ZnZ.of_int i in
    let q = ZnZ.of_int q in
    let rec aux = function
      | h :: t when h = i && t <> [] -> t
      | (h :: _) as acc -> aux ((ZnZ.mul q h) :: acc)
      | [] -> failwith "Nah doesn't happen"
    in
    IntSet.of_list (List.map ZnZ.to_int (aux [i]))

  let g i =
    let open FqmX in
    (* The FqmX.to_array works as follow:
      The calculated polynome is a polynome on a polynome fields.
      Due to maths things, every coefficient is a constant polynome (cuz it works, cf <Cours d'algebre Demazure 9.2.3 "Classes cyclotomiques">)
      The to_array just applies to_int on every coefficient. But to_int of a polynome gives its constant coefficient.
      Then it works, we just convert it back to a PF element
     *)
    PF.of_array @@ FqmX.to_array @@ IntSet.fold (fun l acc -> acc *^ (x -^ ((alpha *. one) **^ l))) (sigma i) one

  let full_sigma = List.fold_left (fun acc i -> IntSet.union acc (sigma i)) IntSet.empty (List.init (delta - 1) ((+) 1))
  let full_g =
    let open FqmX in
    PF.of_array @@ FqmX.to_array @@ IntSet.fold (fun l acc -> acc *^ (x -^ ((alpha *. one) **^ l))) full_sigma one
   
  let k = n - IntSet.cardinal full_sigma 

  let encode a =
    if Array.length a <> k then failwith "Only accept messages of length k" else
    let open PF in
    to_array @@ (of_array a) *^ full_g

  let decode ag =
    if Array.length ag > n then failwith "Only decode messages of size n" else
    let (a, _) = PF.euclidean_div (PF.of_array ag) full_g in
    PF.to_array a
end
