open Utils
open Algebric_structures
open Rings
open Fields
open Polynomes
open Matrixes
open Bch
open Channels
open Pbm
open Interleaver

let () = Random.self_init ()

(* ========================================================================== *)
(* 1. CONFIGURATION                                                           *)
(* ========================================================================== *)

let img_width = 200
let img_height = 200

let s = 4
 
let q = 1 lsl s (* 2^s *)

let m = 1
let t_min = 5

(* ========================================================================== *)
(* 2. CONSTRUCTION ALGÉBRIQUE                                                 *)
(* ========================================================================== *)

module F2Params = struct
  module Ring = IntRing
  let p = Ring.of_int 2
end
module F2 = MakeExtendedField(F2Params)
module F2X = MakePoly(F2)

let poly_prim_s = F2X.primitive_polynome s
module Fq = MakePolyExtendedField(struct
  module Ring = F2X
  let p = poly_prim_s
end)
module FqX = MakePoly(Fq)

let n_gen =
  let q_pow_m = int_of_float (float_of_int q ** float_of_int m) in
  q_pow_m - 1

(* let poly_prim_code = FqX.primitive_polynome m *)
(* Bien trop long, complexité de con (phi_n est de degré varphi(n)) *)
let () = Printf.printf "[INFO] Recherche Monte-Carlo (converti en Las-Vegas) (Irréductible + Primitif) pour n=%d...\n" n_gen; flush stdout
let poly_prim_code = FqX.primitive_polynome_proba m (* askip pas irréductible, TODO *)
let () = Printf.printf "[INFO] Polynôme validé : %s\n" (FqX.to_string poly_prim_code); flush stdout

module AutoBCHParams : BCH_PARAM with module FqX = FqX = struct
  module FqX = FqX
  let primitive_p = poly_prim_code
  let delta = (2 * t_min) + 1
end

let () = Printf.printf "[INFO] Construction BCH...\n"
module MyBCH = BchCode(AutoBCHParams)

(* Besoin entrelacement *)
let noisy_channel = Channels.gilbert_elliott
  ~p_gb:0.001 ~p_bg:0.040 ~err_g:0.00 ~err_b:0.50 

(* let noisy_channel = Channels.gilbert_elliott
  ~p_gb:0.03 ~p_bg:0.300 ~err_g:0.00 ~err_b:0.50 *)

(* ========================================================================== *)
(* 3. TEST                                                                    *)
(* ========================================================================== *)

let run_test () =
  let k_syms = MyBCH.k in
  let n_syms = MyBCH.n in

  if k_syms <= 0 then failwith "k <= 0";

  let k_bits = k_syms * s in
  let n_bits = n_syms * s in

  (* Paramètre d'entrelacement (à ajuster selon la violence de ton canal) *)
  let depth = 8 in

  let raw_size_bits = img_width * img_height in
  let num_blocks_initial = (raw_size_bits + k_bits - 1) / k_bits in
  
  (* On force num_blocks à être un multiple de 'depth' *)
  let num_blocks = 
    if num_blocks_initial mod depth = 0 then num_blocks_initial
    else num_blocks_initial + (depth - (num_blocks_initial mod depth))
  in

  let padded_size_bits = num_blocks * k_bits in
  let total_tx_bits = num_blocks * n_bits in
  let rate = float_of_int k_syms /. float_of_int n_syms in

  Printf.printf "\n=== BCH(%d, %d) sur F%d ===\n" n_syms k_syms q;
  Printf.printf "1. STATISTIQUES\n";
  Printf.printf "   - Capacité (t) : %d symboles\n" t_min;
  Printf.printf "   - Bose dist (d_b) : %d symboles\n" MyBCH.db;
  Printf.printf "   - Taux (R)     : %.2f%%\n" (rate *. 100.);
  Printf.printf "   - Débit utile  : %d bits / %d envoyés\n" raw_size_bits total_tx_bits;

  let img_source = Pbm.create_pattern img_width img_height in
  Pbm.save img_source "1_source.pbm";
  let data_source = Utils.complete_array 0 padded_size_bits img_source.data in

  Printf.printf "[...] Encodage\n";
  let encoded_stream = Array.make total_tx_bits 0 in
  for i = 0 to num_blocks - 1 do
    let msg_bits = Array.sub data_source (i * k_bits) k_bits in
    let msg_syms = Bitpacker.pack_bits s msg_bits in
    let code_syms = Utils.complete_array 0 n_syms (MyBCH.encode msg_syms) in
    let code_bits = Bitpacker.unpack_symbols s code_syms in
    Array.blit code_bits 0 encoded_stream (i * n_bits) n_bits
  done;

  (* Printf.printf "[...] Canal (Gilbert-Elliott)\n";
  let noisy_stream = noisy_channel encoded_stream in
  let channel_errors = Channels.count_errors encoded_stream noisy_stream in

  let noisy_visual = Array.make padded_size_bits 0 in
  for i = 0 to num_blocks - 1 do
    let msg_start = (n_syms - k_syms) * s in
    Array.blit noisy_stream (i*n_bits + msg_start) noisy_visual (i*k_bits) k_bits
  done;
  let final_view = Array.sub noisy_visual 0 raw_size_bits in
  Pbm.save (Pbm.of_channel_output img_width img_height final_view) "2_bruitee.pbm";
  let visible_errors = Channels.count_errors img_source.data final_view in *)

  (* ENTRELACEMENT AVANT LE CANAL *)
  Printf.printf "[...] Entrelacement (Profondeur: %d)\n" depth;
  let interleaved_stream = Interleaver.interleave encoded_stream n_bits depth in

  (* Le flux entrelacé avant le canal *)
  (* On prend une fenêtre de la taille de l'image sur le flux physique *)
  let view_interleaved = Array.sub interleaved_stream 0 raw_size_bits in
  Pbm.save (Pbm.of_channel_output img_width img_height view_interleaved) "1_bis_entrelacee_propre.pbm";

  (* LE CANAL BRUITÉ *)
  Printf.printf "[...] Canal (Gilbert-Elliott)\n";
  let noisy_interleaved = noisy_channel interleaved_stream in
  let channel_errors = Channels.count_errors interleaved_stream noisy_interleaved in

  (* Le flux entrelacé frappé par la rafale *)
  let view_noisy_interleaved = Array.sub noisy_interleaved 0 raw_size_bits in
  Pbm.save (Pbm.of_channel_output img_width img_height view_noisy_interleaved) "2_ter_entrelacee_bruitee.pbm";

  (* DÉSENTRELACEMENT AVANT DÉCODAGE *)
  Printf.printf "[...] Désentrelacement (Dispersion des rafales)\n";
  let noisy_stream = Interleaver.deinterleave noisy_interleaved n_bits depth in

  (* Visualisation Bruit Classique (Image désentrelacée, prête pour le décodeur) *)
  let noisy_visual = Array.make padded_size_bits 0 in
  for i = 0 to num_blocks - 1 do
    let msg_start = (n_syms - k_syms) * s in
    Array.blit noisy_stream (i*n_bits + msg_start) noisy_visual (i*k_bits) k_bits
  done;
  let final_view = Array.sub noisy_visual 0 raw_size_bits in
  Pbm.save (Pbm.of_channel_output img_width img_height final_view) "2_bruitee_desentrelacee.pbm";
  let visible_errors = Channels.count_errors img_source.data final_view in

  Printf.printf "[...] Décodage\n";
  let decoded_data = Array.make padded_size_bits 0 in
  let failures = ref 0 in

  for i = 0 to num_blocks - 1 do
    let rx_bits = Array.sub noisy_stream (i * n_bits) n_bits in
    let rx_syms = Bitpacker.pack_bits s rx_bits in

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

    let res_bits = Bitpacker.unpack_symbols s res_syms in
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

