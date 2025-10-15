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

  let n =
    let open Rings.IntRing in
    to_int (exp (of_int q) m) - 1

  module Fqm = MakePolyExtendedField(struct
    module Ring = PF
    let p = P.primitive_p
  end)
  module FqmX = MakePoly(Fqm) 
  let alpha = FqmX.F.of_int 1 (* Just Fqm.x, but now it works with types... *)
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

  let gtemp i =
    let open FqmX in
    FqmX.to_array @@ IntSet.fold (fun l acc -> acc *^ (x -^ ((alpha *. one) **^ l))) (sigma i) one

  let full_sigma = List.fold_left (fun acc i -> IntSet.union acc (sigma i)) IntSet.empty (List.init delta Fun.id)
  let full_g =
    let open FqmX in
    PF.of_array @@ FqmX.to_array @@ IntSet.fold (fun l acc -> acc *^ (x -^ ((alpha *. one) **^ l))) full_sigma one
   
  let k = n - IntSet.cardinal full_sigma 
end
