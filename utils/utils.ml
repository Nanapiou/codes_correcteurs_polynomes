let fast_operation op neutral a n =
  let rec aux acc a n =
    if n = 0 then acc
    else if n mod 2 = 0 then aux acc (op a a) (n / 2)
    else aux (op acc a) (op a a) (n / 2)
  in
  aux neutral a n

let complete_array (elt : 'a) (n : int) (a : 'a array) : 'a array =
  let n' = Array.length a in
  assert (n' <= n);
  let new_a = Array.make n elt in
  Array.blit a 0 new_a 0 n';
  new_a

let map_one_to_two (f : 'a -> ('b, 'c) Either.t) (l : 'a list) :
    'b list * 'c list =
  let rec aux (accl, accr) = function
    | [] -> (accl, accr)
    | h :: t -> begin
        match f h with
        | Either.Left x -> aux (x :: accl, accr) t
        | Either.Right y -> aux (accl, y :: accr) t
      end
  in
  aux ([], []) l

let map_two_to_two (f : 'a -> 'd -> ('b, 'c) Either.t) (l1 : 'a list)
    (l2 : 'd list) : 'b list * 'c list =
  let rec aux (accl, accr) l1 l2 =
    match (l1, l2) with
    | [], [] -> (accl, accr)
    | h1 :: t1, h2 :: t2 -> begin
        match f h1 h2 with
        | Either.Left x -> aux (x :: accl, accr) t1 t2
        | Either.Right y -> aux (accl, y :: accr) t1 t2
      end
    | _ -> failwith "Not same size lists"
  in
  aux ([], []) l1 l2

let array_rev (a : 'a array) : 'a array =
  let n = Array.length a in
  if n = 0 then [||]
  else
    let a' = Array.make n a.(0) in
    for i = 0 to n - 1 do
      a'.(i) <- a.(n - 1 - i)
    done;
    a'

let divisors (n : int) : int list =
  let sup = n |> float_of_int |> sqrt |> floor |> int_of_float in
  let rec aux acc i =
    if i > sup then acc else aux (if n mod i = 0 then i :: acc else acc) (i + 1)
  in
  aux [] 2

let split_array (c : 'a) (n : int) (a : 'a array) : 'a array list =
  let l = Array.length a in
  let rec aux i =
    let li = l - i in
    let a' = Array.make (min n li) c in
    Array.blit a i a' 0 (min n li);
    if li <= n then [ complete_array c n a' ] else a' :: aux (i + n)
  in
  aux 0

let array_to_string (ts : 'a -> string) (a : 'a array) =
  let elements = Array.to_list a |> List.map ts |> String.concat "; " in
  "[| " ^ elements ^ " |]"

let list_to_string (ts : 'a -> string) (l : 'a list) =
  let elements = l |> List.map ts |> String.concat "; " in
  "[ " ^ elements ^ " ]"

let string_to_binary s =
  let n = String.length s in
  Array.init (n * 8) (fun i ->
      let char_idx = i / 8 in
      let bit_idx = 7 - (i mod 8) in
      let code = Char.code s.[char_idx] in
      (code lsr bit_idx) land 1)

let binary_to_string bits =
  let n_bits = Array.length bits in
  if n_bits mod 8 <> 0 then
    failwith "Array should've a length in 8Z";

  let n_chars = n_bits / 8 in
  String.init n_chars (fun i ->
      let code = ref 0 in
      for j = 0 to 7 do
        let bit = bits.((i * 8) + j) in
        code := (!code lsl 1) lor bit
      done;
      Char.chr !code)
