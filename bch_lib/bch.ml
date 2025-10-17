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
  let delta = P.delta 
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

  (* Bose distance. Not the exact minimal distance, but a good approximation. *)
  let db =
    let temp = ref 2 in
    while IntSet.mem !temp full_sigma do
      incr temp
    done;
    !temp

  let t = db / 2    
   
  let k = n - IntSet.cardinal full_sigma 

  let complete size a =
    let full = Array.make size 0 in
    Array.iteri (fun i v -> full.(i) <- v) a;
    full

  let encode a =
    if Array.length a <> k then failwith "Only accept messages of length k" else
    let open PF in
    let ag = to_array @@ (of_array a) *^ full_g in
    complete n ag

  let decode ag =
    if Array.length ag > n then failwith "Only decode messages of length n" else
    let (a, _) = PF.euclidean_div (PF.of_array ag) full_g in
    let a = PF.to_array a in
    complete k a

  let alpha_powers =
    let a = Array.make n Fqm.zero in
    a.(0) <- Fqm.one;
    for i = 1 to n - 1 do
      a.(i) <- Fqm.mul alpha a.(i - 1)
    done;
    a (* Basically contains every element of the field... Maybe use the Chien Search *)
  let sub_alpha_powers = Array.sub alpha_powers 1 (2 * t)
  let correct m =
    let open FqmX in
    let r' = of_array m in
    let s = normalize @@ Array.map (eval r') sub_alpha_powers in
    let rec build_pi ((pim, bim), (pi, bi)) =
      if deg pi < t then (pi, bi)
      else begin
        let (si, pip) = FqmX.euclidean_div pim pi in
        (* Printf.printf "%s = %s * [%s] + %s\n" (to_string pim) (to_string si) (to_string pi) (to_string pip); *)
        (* print_endline @@ string_of_bool (pim = si *^ pi +^ pip); *)
        build_pi @@ ((pi, bi), (pip, bim -^ si *^ bi))
      end
    in
    let (pi, bi) = build_pi ((x **^ (2 * t), zero), (s, one)) in
    (* print_string "Pi: "; *)
    (* print_endline @@ to_string pi; *)
    (* print_string "Bi: "; *)
    (* print_endline @@ to_string bi; *)
    let coef = constant_coeff bi in
    if coef = Fqm.zero then Result.error bi else
    let coef_inv = Fqm.inv coef in
    let sigma = coef_inv *. bi in
    let sigma' = derive sigma in
    let omega = coef_inv *. pi in
    let e = Array.make n Fqm.zero in 
    (* Forney algorithm *)
    Array.iteri (fun i alphap ->
      let alphinv = Fqm.inv alphap in 
      if eval sigma alphinv = Fqm.zero then e.(i) <- Fqm.sub Fqm.zero @@ Fqm.div (eval omega alphinv) (eval sigma' alphinv)
    ) alpha_powers;
    let r = r' -^ e in
    let ag = to_array r in
    Result.ok @@ complete n ag
end
