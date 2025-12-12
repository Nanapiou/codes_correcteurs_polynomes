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

let map_one_to_two (f: 'a -> ('b, 'c) Either.t) (l: 'a list): 'b list * 'c list =
  let rec aux (accl, accr) = function
    | [] -> accl, accr
    | h :: t -> begin match f h with
      | Either.Left x -> aux (x :: accl, accr) t
      | Either.Right y -> aux (accl, y :: accr) t
    end
  in
  aux ([], []) l

let map_two_to_two (f: 'a -> 'd -> ('b, 'c) Either.t) (l1: 'a list) (l2: 'd list): 'b list * 'c list =
  let rec aux (accl, accr) l1 l2 = match l1, l2 with
    | [], [] -> accl, accr
    | h1 :: t1, h2 :: t2 -> begin match f h1 h2 with
      | Either.Left x -> aux (x :: accl, accr) t1 t2
      | Either.Right y -> aux (accl, y :: accr) t1 t2
    end
    | _ -> failwith "Not same size lists"
  in
  aux ([], []) l1 l2

let array_rev (a: 'a array): 'a array = 
  let n = Array.length a in 
  if n = 0 then [||] else
  let a' = Array.make n a.(0) in 
  for i = 0 to n - 1 do 
    a'.(i) <- a.(n - 1 - i)
  done;
  a'

let divisors (n: int): int list = 
  let sup = n |> float_of_int |> sqrt |> floor |> int_of_float in 
  let rec aux acc i =
    if i > sup then acc
    else
      aux (if n mod i = 0 then i :: acc else acc) (i + 1)
  in
  aux [] 2