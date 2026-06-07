(* depth : Le nombre de blocs que l'on mélange ensemble.
  block_size : La taille d'un bloc en bits (n_bits).
*)
let interleave data block_size depth =
  let total_len = Array.length data in
  let super_block_size = block_size * depth in
  let result = Array.make total_len 0 in
  
  let num_super_blocks = total_len / super_block_size in
  
  for i = 0 to num_super_blocks - 1 do
    let offset = i * super_block_size in
    (* Écriture ligne par ligne, lecture colonne par colonne *)
    for row = 0 to depth - 1 do
      for col = 0 to block_size - 1 do
        let src_idx = offset + (row * block_size) + col in
        let dst_idx = offset + (col * depth) + row in
        if src_idx < total_len && dst_idx < total_len then
          result.(dst_idx) <- data.(src_idx)
      done
    done
  done;
  result

let deinterleave data block_size depth =
  let total_len = Array.length data in
  let super_block_size = block_size * depth in
  let result = Array.make total_len 0 in
  
  let num_super_blocks = total_len / super_block_size in
  
  for i = 0 to num_super_blocks - 1 do
    let offset = i * super_block_size in
    (* Opération symétrique exacte *)
    for row = 0 to depth - 1 do
      for col = 0 to block_size - 1 do
        let src_idx = offset + (col * depth) + row in
        let dst_idx = offset + (row * block_size) + col in
        if src_idx < total_len && dst_idx < total_len then
          result.(dst_idx) <- data.(src_idx)
      done
    done
  done;
  result