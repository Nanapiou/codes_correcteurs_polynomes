module type RING = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val external_mul : int -> t -> t
  val exp : t -> int -> t
  val equal : t -> t -> bool
  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
end

module type EUCLIDEAN_RING = sig
  include RING
  val euclidean_div : t -> t -> t * t
  (* We could have use the Euclidean function conecpt, but may be unoptimized for integers *)
  val egcd : t -> t -> t * t * t
end


module IntRing : EUCLIDEAN_RING = struct
  type t = int
  let zero = 0
  let one = 1
  let add = ( + )
  let sub = ( - )
  let mul = ( * )
  let exp = Utils.fast_operation mul 1
  let external_mul n a = Int.mul n a
  let normalize a b =
    let r = a mod b in
    if r < 0 then r + b else r
  let euclidean_div a b = (a / b, normalize a b)
  let rec egcd a b =
    if b = zero then (a, one, zero)
    else begin
      let (q, r) = euclidean_div a b in
      let (g, x, y) = egcd b r in
      (g, y, sub x (mul q y))
    end
  let equal = ( = )
  let of_int = Fun.id
  let to_int = Fun.id
  let to_string = string_of_int
end
