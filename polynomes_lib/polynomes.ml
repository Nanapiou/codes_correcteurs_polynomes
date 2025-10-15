open Fields
open Rings

module Fields = Fields
module Rings = Rings

module type POLY_EUCLIDEAN_RING = sig
  include EUCLIDEAN_RING
  module F : FIELD
  val x : t
  val ( +^ ) : t -> t -> t
  val ( -^ ) : t -> t -> t
  val ( *^ ) : t -> t -> t
  val ( **^ ) : t -> int -> t
  val ( *. ) : F.t -> t -> t
  val eval : t -> F.t -> F.t
  val deg : t -> int
  val leading_coeff : t -> F.t
  val constant_coeff : t -> F.t
  val of_array : int array -> t
end

(* A set of polynomes which is a field, need to works mod P with P irreductible *)
module type POLY_FIELD = sig
  include FIELD
  module F : FIELD 
  val x : t
  val ( +^ ) : t -> t -> t
  val ( -^ ) : t -> t -> t
  val ( *^ ) : t -> t -> t
  val ( **^ ) : t -> int -> t
  val ( *. ) : F.t -> t -> t
  val eval : t -> F.t -> F.t
  val deg : t -> int
  val leading_coeff : t -> F.t
  val constant_coeff : t -> F.t
  val of_array : int array -> t
end

module type POLY_EXTENDED_FIELD_PARAM = sig
  module Ring : POLY_EUCLIDEAN_RING
  val p : Ring.t
end

module type POLY_EXTENDED_FIELD = sig
  include POLY_FIELD
  module Ring : POLY_EUCLIDEAN_RING
  val p : Ring.t
end

module MakePolyExtendedField(P : POLY_EXTENDED_FIELD_PARAM): POLY_EXTENDED_FIELD = struct
  module Ring = P.Ring
  let p = P.p
  type t = Ring.t

  module F = Ring.F
  let x = Ring.of_int 1
  let ( +^ ) = Ring.( +^ )
  let ( -^ ) = Ring.( -^ )
  let ( *^ ) = Ring.( *^ )
  let ( **^ ) = Ring.( **^ )
  let ( *. ) = Ring.( *. )
  let deg = Ring.deg
  let leading_coeff = Ring.leading_coeff
  let constant_coeff = Ring.constant_coeff
  let eval = Ring.eval

  let zero = Ring.zero
  let one = Ring.one

  let normalize x: t = snd (Ring.euclidean_div x p)
  let to_string = Fun.compose Ring.to_string normalize
  let of_array = Fun.compose normalize Ring.of_array

  let add a b = normalize (Ring.add a b)
  let sub a b = normalize (Ring.sub a b)
  let mul a b = normalize (Ring.mul a b)

  let rec egcd a b =
    if b = zero then (a, one, zero)
    else begin
      let (q, r) = Ring.euclidean_div a b in
      (* Printf.printf "%s = %s * (%s) + %s\n" (to_string a) (Ring.to_string b) (to_string q) (to_string r); *)
      let (g, x, y) = egcd b r in
      (g, y, Ring.sub x (Ring.mul q y))
    end

  let inv a =
    let (g, x, _) = egcd a p in
    if not (deg g = 0) then failwith "No inverse (not a field: q is not irreductible)"
    else (F.inv @@ constant_coeff g) *. (normalize x)

  let div a b = mul a (inv b)
  let equal a b = normalize a = normalize b
  let of_int = Fun.compose normalize Ring.of_int
end 

module MakePoly (F : FIELD): POLY_EUCLIDEAN_RING = struct
  module F = F
  type t = F.t array

  let x = [|F.zero; F.one|]
  let one = [|F.one|]
  let zero = [||]
  let of_int x = [|F.zero; F.of_int x|]
  let of_array = Array.map F.of_int

  let deg (p: t): int = Array.length p - 1
  
  let normalize (p: t): t =
    let n = ref (Array.length p) in
    while !n > 0 && p.(!n - 1) = F.zero do
      decr n
    done;
    Array.sub p 0 !n

  let leading_coeff (p: t) =
    let p = normalize p in
    if Array.length p = 0 then F.zero else p.(deg p)

  let constant_coeff (p: t) =
    let p = normalize p in
    p.(0)

  let equal a b = normalize a = normalize b

  let ( +^ ) (p: t) (q: t): t =
    let n = max (Array.length p) (Array.length q) in
    Array.init n (fun i ->
      let a = if i < Array.length p then p.(i) else F.zero in
      let b = if i < Array.length q then q.(i) else F.zero in
      F.add a b)
    |> normalize
  let add = ( +^ )

  let ( -^ ) (p: t) (q: t): t =
    let n = max (Array.length p) (Array.length q) in
    Array.init n (fun i ->
      let a = if i < Array.length p then p.(i) else F.zero in
      let b = if i < Array.length q then q.(i) else F.zero in
      F.sub a b)
    |> normalize
  let sub = ( -^ )

  let ( *^ ) (p: t) (q: t): t =
    let n = Array.length p and m = Array.length q in
    let r = Array.make (n + m - 1) F.zero in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        r.(i + j) <- F.add r.(i + j) (F.mul p.(i) q.(j))
      done
    done;
    normalize r
  let mul = ( *^ )

  let ( *. ) (a: F.t) (p: t): t = Array.map (F.mul a) p |> normalize

  let ( **^ ) a n: t =
    let rec aux acc a n =
      if n = 0 then acc
      else if n mod 2 = 0 then aux acc (a *^ a) (n / 2)
      else aux (acc *^ a) (a *^ a) (n / 2)
    in
    aux one a n |> normalize
  let exp = ( **^ )
    
  let eval (p: t) (x: F.t): F.t =
    Array.fold_right (fun coeff acc -> F.add coeff (F.mul x acc)) p F.zero
   
  let euclidean_div (a: t) (b: t): t * t =
    let a = normalize a in
    let b = normalize b in
    let degb = deg b in
    if degb < 0 then failwith "Don't divide polynomes by 0" else
    let q = ref zero in
    let r = ref a in
    while deg !r >= degb do
      let fact = (F.div (leading_coeff !r) (leading_coeff b)) *. x **^ (deg !r - degb) in 
      r := !r -^ fact *^ b;
      q := !q +^ fact
    done;
    (normalize !q, normalize !r) (* I still don't fucking know why it needs to be normalized again here *)
    
  let to_string (p: t): string =
    if Array.length p = 0 then (F.to_string F.zero)
    else
      let terms =
        Array.mapi (fun i c ->
          if c = F.zero then None
          else
            Some (
              if i = 0 then F.to_string c
              else if i = 1 then Printf.sprintf "%sx" (F.to_string c)
              else Printf.sprintf "%sx^%d" (F.to_string c) i
            )
        ) p
        |> Array.to_list
        |> List.filter_map (fun x -> x)
      in
      String.concat " + " terms
end
   




