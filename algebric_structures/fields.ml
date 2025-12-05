open Rings

module type FIELD = sig
  include RING
  val inv : t -> t
  val div : t -> t -> t
  val order : int (* -1 if infinite *)
end

module FloatField: FIELD = struct
  type t = float
  let zero = 0.
  let one = 1.
  let add = ( +. )
  let external_mul n f = float_of_int n *. f
  let sub = ( -. )
  let mul = ( *. )
  let exp a n = Float.pow a (float_of_int n)
  let inv x = 1. /. x
  let div = ( /. )
  let equal = (=)
  let of_int = float_of_int
  let to_int = int_of_float
  let to_string = string_of_float

  let order = -1
end

module type EXTENDED_FIELD_PARAM = sig
  module Ring : EUCLIDEAN_RING
  val p : Ring.t  (* The irreducible element *)
end

module type EXTENDED_FIELD = sig
  include FIELD
  module Ring : EUCLIDEAN_RING
  val p : Ring.t 
end

module MakeExtendedField (P : EXTENDED_FIELD_PARAM): EXTENDED_FIELD = struct
  module Ring = P.Ring
  let p = P.p
  type t = Ring.t

  let zero = Ring.zero
  let one = Ring.one

  let normalize x: t = snd (Ring.euclidean_div x p)
  let to_string = Fun.compose Ring.to_string normalize

  let add a b = normalize (Ring.add a b)
  let sub a b = normalize (Ring.sub a b)
  let mul a b = normalize (Ring.mul a b)
  let external_mul n a = Utils.fast_operation add zero a n

  let exp a n: t = Utils.fast_operation mul one a n

  let inv a =
    if a = zero then raise Division_by_zero else
    let (g, x, _) = Ring.egcd a p in
    if not (Ring.equal g one) then failwith "No inverse (not a field: q is not irreductible)"
    else normalize x

  let div a b = mul a (inv b)
  let equal a b = normalize a = normalize b
  let of_int = Fun.compose normalize Ring.of_int
  let to_int = Ring.to_int

  let order = Ring.to_int p
end
