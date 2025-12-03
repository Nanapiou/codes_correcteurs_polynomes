open Polynomes

module IntSet = Set.Make(Int)
let print_int_set s =
  print_char '{';
  IntSet.iter (Printf.printf "%d, ") s;
  print_endline "}"

(* n is the length of the code, q the order of the corpse we work on, r the order of q mod n. We want n maximal, ie n = q^r - 1  *)
(* type tnqr = 
  | NQ of int * int 
  | QR of int * int 
  | NR of int * int 
  | NQR of int * int * int *)

module type BCH_PARAM = sig
  module FqX : POLY_EUCLIDEAN_RING 
  val primitive_p: FqX.t (* An irreductible factor of phi_{q^m-1}, "polynome primitif de degré m" *)

  (* val m : int (* Should be deg(primitive_p) *) *)
  val delta : int
end


module BchCode(P: BCH_PARAM) = struct
  module FqX = P.FqX
  module Fq = FqX.F
  let delta = P.delta 
  let q = Fq.order
  let () = if q = -1 then failwith "Do not use a BCH on a non-finite field."
  let m = FqX.deg P.primitive_p

  let n =
    let open Rings.IntRing in
    to_int (exp (of_int q) m) - 1

  module Fqm = MakePolyExtendedField(struct
    module Ring = FqX
    let p = P.primitive_p
  end)
  module FqmX = MakePoly(Fqm) 
  let alpha = Fqm.x
  (* Alpha is now a root of primitive_p *)
  
  module ZnZ = Fields.MakeExtendedField(struct (* Not a field but I swear I don't use inv *)
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

  let g i: FqX.t =
    let open FqmX in
    Array.map Fqm.constant_coeff @@ IntSet.fold (fun l acc -> acc *^ (x -^ ((alpha *. one) **^ l))) (sigma i) one

  let full_sigma = List.fold_left (fun acc i -> IntSet.union acc (sigma i)) IntSet.empty (List.init (delta - 1) ((+) 1))
  (* Generator of the code *)
  let full_g: FqX.t =
    let open FqmX in
    Array.map Fqm.constant_coeff @@ IntSet.fold (fun l acc -> acc *^ (x -^ ((alpha *. one) **^ l))) full_sigma one

  (* Bose distance. Not the exact minimal distance, but a good approximation. *)
  let db =
    let temp = ref 2 in
    while IntSet.mem !temp full_sigma do
      incr temp
    done;
    !temp

  (* Number of errors it can correct *)
  let t = db / 2    
   
  let k = n - IntSet.cardinal full_sigma 

  let complete size a =
    let full = Array.make size 0 in
    Array.iteri (fun i v -> full.(i) <- v) a;
    full

  let encode_mul a =
    let open FqX in 
    if deg a >= k then failwith "Only accept messages of length k" else
    a *^ full_g 

  let decode_mul ag =
    let open FqX in 
    if deg ag >= n then failwith "Only decode messages of length n" else
    let (a, _) = FqX.euclidean_div ag full_g in
    a

  let xnk = FqX.exp FqX.x (n - k)
  let encode_sys a =
    let open FqX in 
    if deg a >= k then failwith "Only accept messages of length k" else
    let temp = a *^ xnk in
    let (_, r) = euclidean_div temp full_g in 
    temp -^ r

  let decode_sys p: FqX.t = 
    let open FqX in
    if deg p >= n then failwith "Only decode messages of length n" else
    Array.sub p (n - k) k

  let encode = encode_sys 
  let decode = decode_sys
  

  let alpha_powers =
    let a = Array.make n Fqm.zero in
    a.(0) <- Fqm.one;
    for i = 1 to n - 1 do
      a.(i) <- Fqm.mul alpha a.(i - 1)
    done;
    a (* Basically contains every element of the field... Maybe use the Chien Search (it mays not be worth it, we're not on a hardware plan) *)
  let sub_alpha_powers = Array.sub alpha_powers 1 (2 * t)
  (* Forney and Sugiyama algorithm *)
  let correct (r': FqX.t): (FqX.t, FqmX.t) result=
    let open FqmX in
    (* Considering it as a Fqm[X] element in order to calculate syndromes *)
    let r': FqmX.t = Array.map (fun c -> FqX.( *. ) c FqX.one) r' in
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
    let e: FqmX.t = Array.make n Fqm.zero in 
    Array.iteri (fun i alphap ->
      let alphinv = Fqm.inv alphap in 
      if eval sigma alphinv = Fqm.zero then e.(i) <- Fqm.sub Fqm.zero @@ Fqm.div (eval omega alphinv) (eval sigma' alphinv)
    ) alpha_powers;
    Result.ok @@ Array.map Fqm.constant_coeff (r' -^ e)
end
