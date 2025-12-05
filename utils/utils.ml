let fast_operation op neutral a n =
  let rec aux acc a n =
    if n = 0 then acc
    else if n mod 2 = 0 then aux acc (op a a) (n / 2)
    else aux (op acc a) (op a a) (n / 2)
  in
  aux neutral a n

let complete_array (elt: 'a) (n: int) (a: 'a array): 'a array =
  let n' = Array.length a in 
  assert (n' <= n);
  let new_a = Array.make n elt in
  Array.blit a 0 new_a 0 n';
  new_a
