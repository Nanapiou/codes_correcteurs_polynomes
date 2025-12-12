open Algebric_structures
open Fields
open Rings
open Matrixes

module Fields = Fields
module Rings = Rings

module type POLY_EUCLIDEAN_RING = sig
  module F : FIELD
  include EUCLIDEAN_RING with type t = F.t array
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
  val normalize : t -> t
  val derive : t -> t
  val reciprocal : t -> t
  val cyclotomic : int -> t
  val primitive_polynome : int -> t
  val berlekamp : t -> t * t list
  val berlekamp_irreductible : t -> t * t list
  val of_array : int array -> t
  val to_array : t -> int array
end

(* A set of polynomes which is a field, need to works mod P with P irreductible *)
module type POLY_FIELD = sig
  include POLY_EUCLIDEAN_RING
  include FIELD with type t = F.t array
end

module type POLY_EXTENDED_FIELD_PARAM = sig
  module Ring : POLY_EUCLIDEAN_RING
  val p : Ring.t
end

module type POLY_EXTENDED_FIELD = sig
  module Ring : POLY_EUCLIDEAN_RING
  include POLY_FIELD with module F := Ring.F
  val p : Ring.t
end

module MakePolyExtendedField(P : POLY_EXTENDED_FIELD_PARAM): POLY_EXTENDED_FIELD with module Ring = P.Ring = struct
  module Ring = P.Ring
  let p = P.p
  type t = Ring.t

  module F = Ring.F
  let x = Ring.x
  let deg = Ring.deg
  let leading_coeff = Ring.leading_coeff
  let constant_coeff = Ring.constant_coeff
  let eval = Ring.eval
  let reciprocal = Ring.reciprocal
  let to_array = Ring.to_array
  let to_int = Ring.to_int

  let zero = Ring.zero
  let one = Ring.one

  let normalize x: t = snd (Ring.euclidean_div x p)
  let to_string = Fun.compose Ring.to_string normalize
  let of_array = Fun.compose normalize Ring.of_array

  let ( +^ ) a b = normalize @@ Ring.( +^ ) a b
  let ( -^ ) a b = normalize @@ Ring.( -^ ) a b
  let ( *^ ) a b = normalize @@ Ring.( *^ ) a b
  let ( **^ ) a b = normalize @@ Ring.( **^ ) a b
  let ( *. ) a b = normalize @@ Ring.( *. ) a b
  let add a b = normalize (Ring.add a b)
  let sub a b = normalize (Ring.sub a b)
  let mul a b = normalize (Ring.mul a b)
  let external_mul n a = normalize (Ring.external_mul n a)
  let exp a n = Utils.fast_operation mul one a n
  let derive = Fun.compose normalize Ring.derive
  let cyclotomic = Fun.compose normalize Ring.cyclotomic
  let primitive_polynome = Fun.compose normalize Ring.primitive_polynome
  let berlekamp p =
    let coef, l = Ring.berlekamp p in
    coef, (List.map normalize l)
  let berlekamp_irreductible p =
    let coef, l = Ring.berlekamp_irreductible p in
    coef, (List.map normalize l)

  let egcd = Ring.egcd

  let inv a =
    if a = zero then raise Division_by_zero else
    let (g, x, _) = egcd a p in
    if not (deg g = 0) then failwith "No inverse (not a field: p is not irreductible)"
    else (F.inv @@ constant_coeff g) *. (normalize x)

  let div a b = mul a (inv b)
  let equal a b = normalize a = normalize b
  let of_int = Fun.compose normalize Ring.of_int
  let euclidean_div a b = let (q, r) = Ring.euclidean_div a b in (normalize q, normalize r)

  let order =
    if F.order = -1 then -1
    else IntRing.to_int @@ IntRing.exp (IntRing.of_int F.order) (deg p)
end

module rec MakePoly: functor (F : FIELD) ->  POLY_EUCLIDEAN_RING with module F = F = functor (F: FIELD) -> struct
  module F = F
  type t = F.t array

  let x = [|F.zero; F.one|]
  let one = [|F.one|]
  let zero = [||]

  let deg (p: t): int = Array.length p - 1

  let normalize (p: t): t =
    let n = ref (Array.length p) in
    while !n > 0 && p.(!n - 1) = F.zero do
      decr n
    done;
    Array.sub p 0 !n

  let to_array = Array.map F.to_int

  let leading_coeff (p: t) =
    let p = normalize p in
    if Array.length p = 0 then F.zero else p.(deg p)

  let constant_coeff (p: t) =
    let p = normalize p in
    if p = zero then F.zero else
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
  let external_mul n a = Array.map (F.external_mul n) a

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
    if n = 0 && m = 0 then [||] else
    let r = Array.make (n + m - 1) F.zero in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        r.(i + j) <- F.add r.(i + j) (F.mul p.(i) q.(j))
      done
    done;
    normalize r
  let mul = ( *^ )

  let ( *. ) (a: F.t) (p: t): t = Array.map (F.mul a) p |> normalize

  let ( **^ ) a n: t = Utils.fast_operation mul one a n |> normalize
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

  let rec egcd a b =
    if b = zero then (a, one, zero)
    else begin
      let (q, r) = euclidean_div a b in
      (* Printf.printf "%s = %s * (%s) + %s\n" (to_string a) (Ring.to_string b) (to_string q) (to_string r); *)
      let (g, x, y) = egcd b r in
      let inv_g_coef = F.inv @@ leading_coeff g in (* We want the gcd to be unitary *)
      let trans = ( *. ) inv_g_coef in
      (trans g, trans y, trans @@ x -^ q *^ y)
    end

  let derive p: t =
    let p' = Array.mapi F.external_mul p in
    Array.sub p' 1 (deg p)

  let reciprocal (p: t): t =
    let d = deg p in
    let p' = Array.make (d + 1) F.zero in
    for i = 0 to d do
      p'.(d - i) <- p.(i)
    done;
    normalize p'


  let to_int p =
    if F.order = -1 then F.to_int (constant_coeff p) else
    let q = F.order in
    Array.fold_right (fun coeff acc -> F.to_int coeff + (acc * q)) p 0

  let of_int a =
    if F.order = -1 then [|F.of_int a|] else
    let q = F.order in
    let rec build_list a =
      if a = 0 then []
      else
        let (k, l) = (a / q, a mod q) in
        F.of_int l :: build_list k
    in
    Array.of_list @@ build_list a

  let of_array = Array.map F.of_int

  let to_string (p: t): string =
    if Array.length p = 0 then (F.to_string F.zero)
    else
      let terms =
        Array.mapi (fun i c ->
          if c = F.zero then None
          else
            Some (
              if i = 0 then Printf.sprintf "(%s)" (F.to_string c)
              else if i = 1 then Printf.sprintf "(%s)x" (F.to_string c)
              else Printf.sprintf "(%s)x^%d" (F.to_string c) i
            )
        ) p
        |> Array.to_list
        |> List.filter_map Fun.id
      in
      String.concat " + " terms

  let rec cyclotomic (n: int): t = 
    if n = 1 then x -^ one else
    let preds = List.fold_left (fun acc i -> acc *^ cyclotomic i) (cyclotomic 1) (Utils.divisors n) in 
    fst (euclidean_div (x **^ n -^ one) preds)

    
  let berlekamp p: t * t list =
    let p_coef = leading_coeff p in 
    let p = (F.inv p_coef) *. p in
    let q = F.order in
    assert (q <> -1);
    let n = deg p in
    let modp x: t = snd (euclidean_div x p) in
    let mul_modp a b = modp (mul a b) in
    let exp_modp a n = Utils.fast_operation mul_modp one a n in
    let module MnFq = MakeMatrixes(struct
      module F = F
      let n = n
    end) in
    let s a = (exp_modp a q) -^ a in
    let mt: MnFq.t = Array.make n [||] in 
    let x_pow = ref one in
    for i = 0 to n - 1 do 
      mt.(i) <- Utils.complete_array F.zero n @@ s !x_pow;
      x_pow := mul_modp !x_pow x
    done;
    let m = MnFq.transpose mt in
    let g = MnFq.kernel_element m in 
    (* Printf.printf "g: %s\n" (to_string g);
    Printf.printf "S(g): %s\n" (to_string @@ modp (s g));
    Printf.printf "m:\n%s" @@ MnFq.to_string m;
    print_endline @@ MnFq.vector_to_string (MnFq.apply m g); *)
    if Array.exists (( <> ) F.zero) g then
      (p_coef *. one), List.filter_map (fun alpha' ->
        let aplha = F.of_int alpha' in 
        let (gcd, _, _) = egcd p (g -^ aplha *. one) in 
        if deg gcd > 0 && deg gcd < n then Some gcd else None
      ) (List.init q Fun.id)
    else
      (p_coef *. one), [p]

  let berlekamp_irreductible (p: t): t * t list = 
    let rec aux acc_irr = function
      | [] -> acc_irr 
      | l ->
        let l' = List.map berlekamp l in 
        let irr, others = Utils.map_one_to_two (fun (_, factors) ->
          match factors with
          | [q] -> Either.left q 
          | factors -> Either.right factors
        ) l' in
        aux (irr @ acc_irr) (List.concat others)
    in
    (* I want my polynomes to be unitary *)
    let coef, factors = berlekamp p in
    coef, aux [] factors

  let primitive_polynome n =
    n |> cyclotomic |> berlekamp_irreductible |> snd |> List.hd
end


