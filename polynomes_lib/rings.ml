module type EUCLIDEAN_RING = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val external_mul : int -> t -> t
  val exp : t -> int -> t
  val euclidean_div : t -> t -> t * t
  (* We could have use the Euclidean function conecpt, but may be unoptimized for integers *)
  val equal : t -> t -> bool
  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
end


module IntRing : EUCLIDEAN_RING = struct
  type t = int
  let zero = 0
  let one = 1
  let add = ( + )
  let sub = ( - )
  let mul = ( * )
  let exp =
    let rec aux acc a n =
      if n = 0 then acc
      else if n mod 2 = 0 then aux acc (mul a a) (n / 2)
      else aux (mul acc a) (mul a a) (n / 2)
    in
    aux 1
  let external_mul n a =
    let rec aux acc a n =
      if n = 0 then acc
      else if n mod 2 = 0 then aux acc (add a a) (n / 2)
      else aux (add a acc) (add a a) (n / 2)
    in
    aux zero a n
  let normalize a b =
    let r = a mod b in
    if r < 0 then r + b else r
  let euclidean_div a b = (a / b, normalize a b)
  let equal = ( = )
  let of_int = Fun.id
  let to_int = Fun.id
  let to_string = string_of_int
end
