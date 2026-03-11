let bits_to_int bits =
  let acc = ref 0 in
  for i = 0 to Array.length bits - 1 do
    acc := (!acc lsl 1) lor bits.(i)
  done;
  !acc

let int_to_bits v num_bits =
  Array.init num_bits (fun i -> (v lsr (num_bits - 1 - i)) land 1)

(* Pack bits:
  pack_bits 3 [|0; 1; 1; 1; 0; 1|] -> [|3; 5|]
  cuz [|0; 1; 1|] is 3 and [|1; 0; 1|] is 5
*)
let pack_bits s bits =
  if s = 1 then bits
  else
    let num_bits = Array.length bits in
    if s = 0 then [||]
    else
      let num_syms = num_bits / s in
      Array.init num_syms (fun i ->
          let chunk = Array.sub bits (i * s) s in
          bits_to_int chunk)

(* Inverse of pack_bits *)
let unpack_symbols s syms =
  if s = 1 then syms
  else
    let num_syms = Array.length syms in
    let bits = Array.make (num_syms * s) 0 in
    for i = 0 to num_syms - 1 do
      let b = int_to_bits syms.(i) s in
      Array.blit b 0 bits (i * s) s
    done;
    bits
