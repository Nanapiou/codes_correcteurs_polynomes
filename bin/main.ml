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
  let p = primitive_polynome 15
end)
module F16X = MakePoly(F16)

module RS15_11 = BchCode(struct
  module FqX = F16X

  let delta = 3

  open FqX
  (* let m = 1 *)
  (* let primitive_p = x -^ (F16.x *. one) *)
  let primitive_p = primitive_polynome 15
end)

module H7_4 = BchCode(struct (* Hamming code (7, 4) *)
  module FqX = F2X
    
  let delta = 2 
  open FqX 

  let primitive_p = primitive_polynome 7
  (* let m = 3 *)
end)

module Bch15 = BchCode(struct
  module FqX = F2X
  (* let m = 4 *)
  let delta = 2
  open FqX
  let primitive_p = primitive_polynome 15
  (* let m = 4 *)
end)


(* rs_random_test.ml *)

open Printf
open Unix

open RS15_11
open F16X      (* your polynomial module *)

(* ===== Parameters ===== *)
let trials_per_weight = 500    (* adjust for more stats *)
let max_errors_to_test = 6
let random_seed = 41
let do_bursts = false 
(* ====================== *)

let () = Random.init random_seed

(* ---- Helpers ---- *)

(* A zero polynomial that is valid for any implementation: *)
let zero = of_array [|0|]

(* Random non-zero coefficient in F16 *)
let rand_nonzero_field () =
  let v = 1 + Random.int (16 - 1) in
  F16.of_int v

(* Build error polynomial from chosen positions *)
let error_of_positions positions =
  List.fold_left (fun acc pos ->
    let coeff = rand_nonzero_field () in
    acc +^ (coeff *. (x **^ pos))
  ) zero positions

(* Distinct random error positions *)
let random_positions weight =
  let rec go acc =
    if List.length acc = weight then acc
    else
      let p = Random.int n in
      if List.mem p acc then go acc else go (p :: acc)
  in
  if weight = 0 then [] else go []

(* Burst positions, contiguous *)
let burst_positions weight =
  if weight = 0 then []
  else
    let start = Random.int n in
    let rec go i k acc =
      if k = 0 then acc
      else go (i+1) (k-1) (((start+i) mod n)::acc)
    in go 0 weight []

(* Remainder mod generator polynomial *)
let remainder p =
  snd (euclidean_div p full_g)

(* One decoding attempt *)
let run_single_trial msg weight =
  let ef = encode msg in
  let positions =
    if do_bursts then burst_positions weight
    else random_positions weight
  in
  let err = error_of_positions positions in
  let errored = ef +^ err in
  match correct errored with
  | Result.Error _ -> false
  | Result.Ok cef ->
      (* Preferred equality check: compare remainder mod g *)
      (remainder cef) = (remainder ef)

(* Run many trials; report success & time *)
let run_trials_for_weight msg weight trials =
  let t0 = gettimeofday () in
  let rec loop k successes =
    if k = 0 then successes
    else
      let ok = run_single_trial msg weight in
      loop (k-1) (if ok then successes + 1 else successes)
  in
  let succ = loop trials 0 in
  let t1 = gettimeofday () in
  (succ, trials, t1 -. t0)

(* Build a random message polynomial *)
let random_message () =
  let max_deg = 8 in        (* restraint; RS encoder will handle exact k *)
  let d = Random.int max_deg in
  let rec build i acc =
    if i < 0 then acc
    else
      let c = rand_nonzero_field () in
      build (i-1) (acc +^ (c *. (x **^ i)))
  in build d zero


let clean_poly_to_string p =
  let p': RX.t = Array.map (Fun.compose Fields.FloatField.of_int F2X.to_int) p in 
  RX.to_string p'
(* Full test over several messages *)
let test_random_messages ~samples =
  printf "Random RS test: %d msgs, %d trials/weight\nPrimitive polynome: %s\nq = %d, m = %d, n = %d\n%!"
    samples trials_per_weight (clean_poly_to_string primitive_p) q m n;
  for m = 1 to samples do
    let msg = random_message () in
    printf "\nMessage %d:\n%!" m;
    for w = 0 to max_errors_to_test do
      let (succ, total, dt) =
        run_trials_for_weight msg w trials_per_weight
      in
      let pct =
        Float.mul 100. (float succ) /. float total
      in
      printf "  weight=%2d -> %4d/%4d = %6.2f%%   dt=%.3fs\n%!"
        w succ total pct dt
    done
  done

let () =
  test_random_messages ~samples:3;
  printf "\nDone.\n%!"
