open Rings

module type FIELD = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val inv : t -> t
  val div : t -> t -> t
  val equal : t -> t -> bool
  val of_int : int -> t
  val to_string : t -> string
end

module FloatField : FIELD = struct
  type t = float
  let zero = 0.
  let one = 1.
  let add = ( +. )
  let sub = ( -. )
  let mul = ( *. )
  let inv x = 1. /. x
  let div = ( /. )
  let equal = (=)
  let of_int = float_of_int
  let to_string = string_of_float
end

module type EXTENDED_FIELD_PARAM = sig
  module Ring : EUCLIDEAN_RING
  val p : Ring.t  (* The irreducible element *)
end

module MakeExtendedField (P : EXTENDED_FIELD_PARAM): FIELD = struct
  module Ring = P.Ring
  let p = P.p
  type t = Ring.t

  let zero = Ring.zero
  let one = Ring.one

  let normalize x: t = snd (Ring.euclidean_div x p)

  let add a b = normalize (Ring.add a b)
  let sub a b = normalize (Ring.sub a b)
  let mul a b = normalize (Ring.mul a b)

  let rec egcd a b =
    if b = zero then (a, one, zero)
    else
      let (q, r) = Ring.euclidean_div a b in
      let (g, x, y) = egcd b r in
      (g, y, Ring.sub x (Ring.mul q y))

  let inv a =
    let (g, x, _) = egcd a p in
    if not (Ring.equal g one) then failwith "No inverse (not a field: q is not irreductible)"
    else normalize x

  let div a b = mul a (inv b)
  let equal a b = normalize a = normalize b
  let of_int = Fun.compose normalize Ring.of_int
  let to_string = Fun.compose Ring.to_string normalize
end
