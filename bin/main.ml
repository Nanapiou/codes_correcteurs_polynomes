(* ========================================================================== *)
(* TIPE : LA DÉMULTIPLICATION (F_q -> F_p^s) CONTRE LES ERREURS EN PAQUETS    *)
(* Concept : Une rafale de bits dans le canal ne compte que pour peu          *)
(* d'erreurs dans le corps de Galois F_256.                                   *)
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
(* 1. SETUP ALGÉBRIQUE : RS(255, 223) sur GF(256)                             *)
(* ========================================================================== *)

(* Base F2 *)
module F2Params = struct
  module Ring = IntRing
  let p = Ring.of_int 2 
end
module F2 = MakeExtendedField(F2Params)
module F2X = MakePoly(F2)

(* Extension GF(256) *)
module GF256Params = struct
  module Ring = F2X
  (* P(X) = X^8 + X^4 + X^3 + X^2 + 1 (0x11D) *)
  let p = F2X.of_array [| 1; 0; 1; 1; 1; 0; 0; 0; 1 |]
end
module GF256 = MakeExtendedField(GF256Params)
module GF256X = MakePoly(GF256)

(* Paramètres RS *)
module RS_Params : BCH_PARAM with module FqX = GF256X = struct
  module FqX = GF256X
  (* P(Y) = Y + 2 (alpha) *)
  let primitive_p = GF256X.of_array [| 2; 1 |] 
  (* t=16 erreurs => delta=33 *)
  let delta = 33
end

module MyRS = BchCode(RS_Params)


(* ========================================================================== *)
(* 2. MODULE DE DÉMULTIPLICATION (L'ISOMORPHISME)                             *)
(* ========================================================================== *)

module Demultiplication = struct
  (* Transforme un flux de bits en flux de symboles (GF256) *)
  (* phi^-1 : (F_2)^8 -> F_256 *)
  let bits_to_symbols (bits : int array) : int array =
    let n_bits = Array.length bits in
    (* Padding si pas multiple de 8 *)
    let n_syms = (n_bits + 7) / 8 in
    let symbols = Array.make n_syms 0 in
    
    for i = 0 to n_syms - 1 do
      let acc = ref 0 in
      for j = 0 to 7 do
        let bit_idx = i * 8 + j in
        let bit = if bit_idx < n_bits then bits.(bit_idx) else 0 in
        (* Construction de l'octet (MSB first ou LSB, peu importe tant qu'on est cohérent) *)
        acc := (!acc lsl 1) lor bit
      done;
      symbols.(i) <- !acc
    done;
    symbols

  (* Transforme un flux de symboles (GF256) en flux de bits *)
  (* phi : F_256 -> (F_2)^8 *)
  let symbols_to_bits (symbols : int array) : int array =
    let n_syms = Array.length symbols in
    let n_bits = n_syms * 8 in
    let bits = Array.make n_bits 0 in
    
    for i = 0 to n_syms - 1 do
      let sym = symbols.(i) in
      for j = 0 to 7 do
        (* Extraction des bits (du MSB au LSB pour correspondre à l'inverse) *)
        let bit = (sym lsr (7 - j)) land 1 in
        bits.(i * 8 + j) <- bit
      done;
    done;
    bits
end


(* ========================================================================== *)
(* 3. SCÉNARIO : RS SUR CANAL BINAIRE À MÉMOIRE                               *)
(* ========================================================================== *)

let run_demo () =
  Channels.init ();
  Printf.printf "=== DÉMULTIPLICATION : RS(255) SUR CANAL BINAIRE ===\n";
  
  (* 1. Paramètres *)
  let n_rs = MyRS.n in (* 255 symboles *)
  let k_rs = MyRS.k in (* 223 symboles *)
  
  (* Taille en BITS d'un bloc encodé *)
  
  (* 2. Image Source (Bits) *)
  Printf.printf "1. Génération Image PBM...\n";
  (* Largeur : on veut exactement K symboles de large pour simplifier la démo *)
  (* k_rs symboles = k_rs * 8 bits *)
  let w_bits = k_rs * 8 in 
  let h = 64 in
  let img_source = Pbm.create_pattern w_bits h in
  Pbm.save img_source "1_originale.pbm";

  Printf.printf "   Image : %dx%d bits.\n" w_bits h;
  Printf.printf "   Utilisation RS(%d, %d) sur GF(256).\n" n_rs k_rs;

  (* 3. Encodage + Démultiplication *)
  Printf.printf "2. Encodage (Isomorphisme F2^8 -> F256 -> RS -> F2^8)...\n";
  
  (* Buffer pour stocker l'image encodée (plus large car redondance) *)
  let w_encoded_bits = n_rs * 8 in
  let bits_transmission = Array.make (w_encoded_bits * h) 0 in
  
  for y = 0 to h - 1 do
    (* A. Extraction ligne (Bits) *)
    let row_bits = Array.sub img_source.data (y * w_bits) w_bits in
    
    (* B. Démultiplication Inverse : Bits -> Symboles *)
    let msg_symbols = Demultiplication.bits_to_symbols row_bits in
    (* msg_symbols doit faire k_rs de long *)
    
    (* C. Encodage RS (Algébrique) *)
    let code_symbols_raw = MyRS.encode msg_symbols in
    let code_symbols = Utils.complete_array 0 n_rs code_symbols_raw in
    
    (* D. Démultiplication : Symboles -> Bits *)
    let code_bits = Demultiplication.symbols_to_bits code_symbols in
    
    (* E. Placement dans le buffer de transmission *)
    (* Note : ici le code n'est pas forcément systématique visuellement en bits *)
    (* car le mélange bits/symboles peut être complexe selon le polynôme *)
    (* Mais on copie tout le bloc encodé *)
    Array.blit code_bits 0 bits_transmission (y * w_encoded_bits) w_encoded_bits;
  done;
  
  let img_encoded = Pbm.of_channel_output w_encoded_bits h bits_transmission in
  Pbm.save img_encoded "2_encoded_stream.pbm";


  (* 4. Canal Gilbert-Elliott (Sur les BITS !) *)
  Printf.printf "3. Transmission Canal Binaire (Rafales de bits)...\n";
  
  (* SCÉNARIO CRITIQUE : *)
  (* On génère des rafales de bits de longueur moyenne 10-20 bits. *)
  (* Pour un code binaire t=2, ce serait mortel. *)
  (* Pour RS sur octets, 20 bits = max 3 ou 4 octets touchés. *)
  (* RS corrige 16 symboles, donc c'est TRÈS facile pour lui. *)
  
  let bits_received = Channels.gilbert_elliott 
    ~p_gb:0.005  (* Incidents rares *)
    ~p_bg:0.10   (* Rafales longues ! (1/0.10 = 10 bits moy) *)
    ~err_g:0.0 
    ~err_b:0.5 
    bits_transmission
  in
  
  let img_noisy = Pbm.of_channel_output w_encoded_bits h bits_received in
  Pbm.save img_noisy "3_noisy_stream.pbm";
  
  let nb_bit_errors = Channels.count_errors bits_transmission bits_received in
  Printf.printf "   -> %d bits erronés.\n" nb_bit_errors;


  (* 5. Décodage *)
  Printf.printf "4. Réception & Décodage...\n";
  let bits_corrected = Array.make (w_bits * h) 0 in
  
  let total_symbol_errors = ref 0 in
  
  for y = 0 to h - 1 do
    (* A. Lecture ligne bruitée (Bits) *)
    let row_received_bits = Array.sub bits_received (y * w_encoded_bits) w_encoded_bits in
    
    (* B. Démultiplication Inverse : Bits -> Symboles *)
    (* C'est là que la "compression d'erreurs" se fait : *)
    (* 5 bits faux à la suite peuvent tomber dans le même symbole *)
    let received_symbols = Demultiplication.bits_to_symbols row_received_bits in
    
    (* C. Correction RS *)
    let corrected_symbols = 
      match MyRS.correct received_symbols with
      | Some c -> 
          (* Statistique pour le TIPE : compter combien de symboles ont été changés *)
          (* pour comparer au nombre de bits *)
          let diff = ref 0 in
          Array.iter2 (fun a b -> if a <> b then incr diff) received_symbols (Utils.complete_array 0 n_rs c);
          total_symbol_errors := !total_symbol_errors + !diff;
          c
      | None -> received_symbols (* Echec *)
    in
    
    let full_corrected = Utils.complete_array 0 n_rs corrected_symbols in
    
    (* D. Extraction Message (supposons systématique sur les symboles) *)
    (* Dans BchCode, systématique => Message à la fin (degrés élevés) *)
    let msg_symbols = Array.sub full_corrected (n_rs - k_rs) k_rs in
    
    (* E. Démultiplication : Symboles -> Bits *)
    let msg_bits = Demultiplication.symbols_to_bits msg_symbols in
    
    (* Ecriture *)
    Array.blit msg_bits 0 bits_corrected (y * w_bits) w_bits;
  done;

  let img_final = Pbm.of_channel_output w_bits h bits_corrected in
  Pbm.save img_final "4_corrected.pbm";
  
  let err_finale = Channels.count_errors img_source.data bits_corrected in
  
  Printf.printf "   STATISTIQUES TIPE :\n";
  Printf.printf "   - Bits corrompus (Physique) : %d\n" nb_bit_errors;
  Printf.printf "   - Symboles corrompus (Algébrique) : %d\n" !total_symbol_errors;
  Printf.printf "   - Ratio (Bits / Symboles) : %.2f\n" (float_of_int nb_bit_errors /. float_of_int !total_symbol_errors);
  Printf.printf "   (Un ratio > 1 prouve l'efficacité de la démultiplication contre les rafales)\n";
  
  Printf.printf "   -> Erreurs résiduelles : %d\n" err_finale

let () = run_demo ()