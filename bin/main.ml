open Polynomes
open Bch

module F2 = Fields.MakeExtendedField(struct
  module Ring = Rings.IntRing
  let p = Ring.of_int 2
end)
module F2X = MakePoly(F2)

module Bch15 = BchCode(struct
  module PF = F2X
  let m = 4
  let delta = 6
  open PF
  let primitive_p = x **^ 4 +^ x +^ one
end)

open Bch15

(* ---------- helpers ---------- *)

let bitstring_of_array a =
  let buf = Buffer.create (Array.length a) in
  Array.iter (fun x -> Buffer.add_char buf (if x = 0 then '0' else '1')) a;
  Buffer.contents buf

let indices_of_ones arr =
  let buf = Buffer.create 32 in
  Buffer.add_char buf '[';
  let first = ref true in
  Array.iteri (fun i v ->
    if v <> 0 then (
      if not !first then Buffer.add_string buf ", ";
      first := false;
      Buffer.add_string buf (string_of_int i)
    )
  ) arr;
  Buffer.add_char buf ']';
  Buffer.contents buf

let string_of_positions pos_arr =
  if Array.length pos_arr = 0 then "[]"
  else
    let buf = Buffer.create (Array.length pos_arr * 3) in
    Buffer.add_char buf '[';
    Array.iteri (fun i v ->
      if i > 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf (string_of_int v)
    ) pos_arr;
    Buffer.add_char buf ']';
    Buffer.contents buf

let shuffle_inplace a =
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp
  done

let choose_positions ~n ~k =
  if k <= 0 then [||]
  else if k >= n then Array.init n (fun i -> i)
  else
    let arr = Array.init n (fun i -> i) in
    shuffle_inplace arr;
    Array.init k (fun i -> arr.(i))

let flip_bits code pos_arr =
  Array.iter (fun p -> code.(p) <- 1 - code.(p)) pos_arr

let random_message () =
  Array.init k (fun _ -> if Random.bool () then 1 else 0)

let print_case ~prefix ~msg ~cw ~corrupted ~corrected ~decoded =
  Printf.printf "\n%s\n" prefix;
  Printf.printf " msg      : %s\n" (bitstring_of_array msg);
  Printf.printf " encoded  : %s\n" (bitstring_of_array cw);
  let diff = Array.init (Array.length cw) (fun i -> if cw.(i) = corrupted.(i) then 0 else 1) in
  Printf.printf " corrupted: %s (errors at %s)\n"
    (bitstring_of_array corrupted) (indices_of_ones diff);
  Printf.printf " corrected: %s\n" (bitstring_of_array corrected);
  Printf.printf " decoded  : %s\n" (bitstring_of_array decoded);
  let success = Array.length decoded = Array.length msg && Array.for_all2 (=) msg decoded in
  Printf.printf " success? %b\n" success

(* ---------- single examples ---------- *)

let example_with_errors e =
  let msg = random_message () in
  let cw = encode msg in
  let corrupted = Array.copy cw in
  let pos = choose_positions ~n:n ~k:e in
  flip_bits corrupted pos;
  let corrected = correct corrupted in
  let decoded = decode corrected in
  Array.sort compare pos; (* sort in-place for nicer printing *)
  Printf.printf "\nExample with %d error(s) (t = %d)\n" e t;
  Printf.printf " error positions: %s\n" (string_of_positions pos);
  print_case ~prefix:"---" ~msg ~cw ~corrupted ~corrected ~decoded

(* ---------- trials to estimate empirical success rate ---------- *)

let trials n_trials errors_per_trial =
  let succ = ref 0 in
  for _ = 1 to n_trials do
    let msg = random_message () in
    let cw = encode msg in
    let corrupted = Array.copy cw in
    let pos = choose_positions ~n:n ~k:errors_per_trial in
    flip_bits corrupted pos;
    let corrected =
      try correct corrupted with Division_by_zero -> begin
        (* print_endline @@ bitstring_of_array corrupted; *)
        corrupted
      end
    in
    let decoded = decode corrected in
    if Array.length decoded = Array.length msg && Array.for_all2 (=) msg decoded then incr succ
  done;
  !succ

(* ---------- main ---------- *)

let () =
  Random.self_init ();
  Printf.printf "BCH parameters: k=%d, delta=%d, n=%d, t=%d\n" k delta n t;
  Printf.printf "generator poly (full_g): %s\n" (F2X.to_string full_g);

  example_with_errors (t - 1);

  (* show one example with exactly t errors (should be corrected) *)
  example_with_errors t;

  (* show one example with t+1 errors (may fail) *)
  example_with_errors (t + 1);

  (* run many trials to compare empirical success for t and t+1 errors *)
  let trials_count = 1000 in
  let succ_t  = trials trials_count t in
  let succ_tp1 = trials trials_count (t + 1) in
  Printf.printf "\nAfter %d random trials:\n" trials_count;
  Printf.printf " corrected when <= t errors : %d / %d (%.2f%%)\n"
    succ_t trials_count (100. *. float_of_int succ_t /. float_of_int trials_count);
  Printf.printf " corrected when t+1 errors  : %d / %d (%.2f%%)\n"
    succ_tp1 trials_count (100. *. float_of_int succ_tp1 /. float_of_int trials_count);
  ()
