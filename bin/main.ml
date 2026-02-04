(* ========================================================================== *)
(* MAIN.ML - VERSION FINALE : PRIMITIVITÉ GARANTIE                            *)
(* ========================================================================== *)

open Utils
open Algebric_structures
open Rings
open Fields
open Polynomes
open Matrixes
open Bch
open Channels
open Pbm

(* ========================================================================== *)
(* 1. CONFIGURATION                                                           *)
(* ========================================================================== *)

let img_width = 64
let img_height = 64

(* s=4 (Hexa) : Optimum pour les rafales courtes *)
let s = 4 

let q = 1 lsl s

let m, t = 
  match s with
  | 1 -> (8, 25) 
  | 4 -> (2, 14) (* n=255, t=14 *)
  | _ -> (2, 2)

(* ========================================================================== *)
(* 2. CONSTRUCTION ALGÉBRIQUE                                                 *)
(* ========================================================================== *)

module F2Params = struct
  module Ring = IntRing
  let p = Ring.of_int 2
end
module F2 = MakeExtendedField(F2Params)
module F2X = MakePoly(F2)

let poly_prim_s = F2X.primitive_polynome ((1 lsl s) - 1)
module Fq = MakePolyExtendedField(struct
  module Ring = F2X
  let p = poly_prim_s
end)
module FqX = MakePoly(Fq)

let n_gen = 
  let q_pow_m = int_of_float (float_of_int q ** float_of_int m) in
  q_pow_m - 1

(* --- NOUVEAU : VERIFICATION DE PRIMITIVITE --- *)
(* Exponentiation modulaire de polynômes : (A^power) mod M *)
let rec poly_pow_mod a power m =
  let open FqX in
  if power = 0 then one
  else if power mod 2 = 0 then
    let t = poly_pow_mod a (power / 2) m in
    let _, r = euclidean_div (t *^ t) m in
    r
  else
    let t = poly_pow_mod a (power - 1) m in
    let _, r = euclidean_div (a *^ t) m in
    r

(* Factorisation basique pour trouver les facteurs premiers de n *)
let get_prime_factors n =
  let rec aux d n acc =
    if n = 1 then acc
    else if n mod d = 0 then aux d (n / d) (d :: acc)
    else aux (d + 1) n acc
  in
  (* On dédoublonne les facteurs *)
  List.sort_uniq Int.compare (aux 2 n [])

(* Test si un polynôme P est primitif pour l'ordre n *)
let is_primitive p n =
  let open FqX in
  let factors = get_prime_factors n in
  (* Condition 1 : X^n = 1 mod P (toujours vrai pour un irréductible de bon degré) *)
  (* Condition 2 : X^(n/f) != 1 mod P pour tout facteur premier f *)
  let x_poly = [|Fq.zero; Fq.one|] in (* Le polynôme X *)
  List.for_all (fun f ->
    let check_deg = n / f in
    let res = poly_pow_mod x_poly check_deg p in
    not (res = one) (* Doit être différent de 1 *)
  ) factors

let find_primitive_poly_robust degree =
  Printf.printf "[INFO] Recherche Monte-Carlo (Irréductible + Primitif) pour n=%d...\n" n_gen;
  let rec attempt () =
    let coeffs = Array.init (degree + 1) (fun i ->
      if i = degree then 1 else Random.int q
    ) in
    if coeffs.(0) = 0 then attempt ()
    else
      let p = FqX.of_array coeffs in
      (* 1. Irréductibilité (Rapide) *)
      let _, factors = FqX.berlekamp p in
      if List.length factors = 1 then
        (* 2. Primitivité (Crucial) *)
        if is_primitive p n_gen then p
        else attempt () (* Irréductible mais pas primitif -> on rejette *)
      else attempt ()
  in
  attempt ()

let poly_prim_code = find_primitive_poly_robust m
let () = Printf.printf "[INFO] Polynôme validé : %s\n" (FqX.to_string poly_prim_code)

module AutoBCHParams : BCH_PARAM with module FqX = FqX = struct
  module FqX = FqX
  let primitive_p = poly_prim_code
  let delta = (2 * t) + 1
end

let () = Printf.printf "[INFO] Construction BCH...\n"
module MyBCH = BchCode(AutoBCHParams)

(* ========================================================================== *)
(* 3. BIT-PACKING                                                             *)
(* ========================================================================== *)

module BitPacker = struct
  let bits_to_int bits =
    let acc = ref 0 in
    for i = 0 to Array.length bits - 1 do
      acc := (!acc lsl 1) lor bits.(i)
    done;
    !acc

  let int_to_bits v num_bits =
    Array.init num_bits (fun i -> (v lsr (num_bits - 1 - i)) land 1)

  let pack_bits bits =
    if s = 1 then bits 
    else
      let num_bits = Array.length bits in
      if s = 0 then [||] else
      let num_syms = num_bits / s in
      Array.init num_syms (fun i ->
        let chunk = Array.sub bits (i * s) s in
        bits_to_int chunk
      )

  let unpack_symbols syms =
    if s = 1 then syms 
    else
      let num_syms = Array.length syms in
      let bits = Array.make (num_syms * s) 0 in
      for i = 0 to num_syms - 1 do
        let b = int_to_bits syms.(i) s in
        Array.blit b 0 bits (i * s) s
      done;
      bits
end

(* ========================================================================== *)
(* 4. TEST                                                                    *)
(* ========================================================================== *)

let run_test () =
  Channels.init ();
  let k_syms = MyBCH.k in
  let n_syms = MyBCH.n in
  
  if k_syms <= 0 then failwith "k <= 0";

  let k_bits = k_syms * s in 
  let n_bits = n_syms * s in

  let raw_size_bits = img_width * img_height in
  let num_blocks = (raw_size_bits + k_bits - 1) / k_bits in 
  let padded_size_bits = num_blocks * k_bits in
  let total_tx_bits = num_blocks * n_bits in
  let rate = float_of_int k_syms /. float_of_int n_syms in

  Printf.printf "\n=== BCH(%d, %d) sur F%d ===\n" n_syms k_syms q;
  Printf.printf "1. STATISTIQUES\n";
  Printf.printf "   - Capacité (t) : %d symboles\n" t;
  Printf.printf "   - Taux (R)     : %.2f%%\n" (rate *. 100.);
  Printf.printf "   - Débit utile  : %d bits / %d envoyés\n" raw_size_bits total_tx_bits;

  let img_source = Pbm.create_pattern img_width img_height in
  Pbm.save img_source "1_source.pbm";
  let data_source = Utils.complete_array 0 padded_size_bits img_source.data in

  Printf.printf "[...] Encodage\n";
  let encoded_stream = Array.make (num_blocks * n_bits) 0 in
  for i = 0 to num_blocks - 1 do
    let msg_bits = Array.sub data_source (i * k_bits) k_bits in
    let msg_syms = BitPacker.pack_bits msg_bits in
    let code_syms = Utils.complete_array 0 n_syms (MyBCH.encode msg_syms) in
    let code_bits = BitPacker.unpack_symbols code_syms in
    Array.blit code_bits 0 encoded_stream (i * n_bits) n_bits
  done;

  Printf.printf "[...] Canal (Gilbert-Elliott)\n";
  let noisy_stream = Channels.gilbert_elliott 
    ~p_gb:0.010 ~p_bg:0.400 ~err_g:0.00 ~err_b:0.50 
    encoded_stream 
  in
  let channel_errors = Channels.count_errors encoded_stream noisy_stream in

  let noisy_visual = Array.make padded_size_bits 0 in
  for i = 0 to num_blocks - 1 do
    let msg_start = (n_syms - k_syms) * s in 
    Array.blit noisy_stream (i*n_bits + msg_start) noisy_visual (i*k_bits) k_bits
  done;
  let final_view = Array.sub noisy_visual 0 raw_size_bits in
  Pbm.save (Pbm.of_channel_output img_width img_height final_view) "2_bruitee.pbm";
  let visible_errors = Channels.count_errors img_source.data final_view in

  Printf.printf "[...] Décodage\n";
  let decoded_data = Array.make padded_size_bits 0 in
  let failures = ref 0 in

  for i = 0 to num_blocks - 1 do
    let rx_bits = Array.sub noisy_stream (i * n_bits) n_bits in
    let rx_syms = BitPacker.pack_bits rx_bits in
    
    let res_syms = 
      try
        match MyBCH.correct rx_syms with
        | Some c -> 
            let full = Utils.complete_array 0 n_syms c in
            Array.sub full (n_syms - k_syms) k_syms
        | None -> 
            incr failures;
            Array.sub rx_syms (n_syms - k_syms) k_syms
      with Division_by_zero ->
        incr failures;
        Array.sub rx_syms (n_syms - k_syms) k_syms
    in
    
    let res_bits = BitPacker.unpack_symbols res_syms in
    Array.blit res_bits 0 decoded_data (i * k_bits) k_bits
  done;

  let final_bits = Array.sub decoded_data 0 raw_size_bits in
  Pbm.save (Pbm.of_channel_output img_width img_height final_bits) "3_corrigee.pbm";
  
  let residual_errors = Channels.count_errors img_source.data final_bits in
  let corrected_count = visible_errors - residual_errors in

  Printf.printf "\n2. RÉSULTATS\n";
  Printf.printf "   - Bruit injecté      : %d bits (%d visibles)\n" channel_errors visible_errors;
  Printf.printf "   - Erreurs corrigées  : %d\n" corrected_count;
  Printf.printf "   - Erreurs restantes  : %d\n" residual_errors;
  
  if residual_errors = 0 then 
    Printf.printf "\n>>> SUCCÈS TOTAL <<<\n"
  else 
    Printf.printf "\n>>> Correction Partielle : %.1f%% <<<\n" 
      (100. *. float_of_int corrected_count /. float_of_int visible_errors)

let () = run_test ()