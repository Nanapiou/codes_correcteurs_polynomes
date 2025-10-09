open Fields
open Rings

module Fields = Fields
module Rings = Rings

module MakePoly (F : FIELD): EUCLIDEAN_RING = struct
  type t = F.t array

  let x = [|F.zero; F.one|]
  let one = [|F.one|]
  let zero = [||]
  let of_int x = [|F.zero; F.of_int x|]

  let deg (p: t): int = Array.length p - 1

  let leading_coeff (p: t) =
    if Array.length p = 0 then F.zero else p.(deg p)

  let normalize (p: t): t =
    let n = ref (Array.length p) in
    while !n > 0 && p.(!n - 1) = F.zero do
      decr n
    done;
    Array.sub p 0 !n

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

  let ( *. ) (a: F.t): t -> t = Array.map (F.mul a) 

  let ( **^ ): t -> int -> t =
    let rec aux acc a n =
      if n = 0 then acc
      else if n mod 2 = 0 then aux acc (a *^ a) (n / 2)
      else aux (acc *^ a) (a *^ a) (n / 2)
    in
    aux one
  let exp = ( **^ )
    
  let eval (p: t) (x: F.t): F.t =
    Array.fold_right (fun coeff acc -> F.add coeff (F.mul x acc)) p F.zero
   
  let euclidean_div (a: t) (b: t): t * t =
    let degb = deg b in
    if degb < 0 then failwith "Don't divide polynomes by 0" else
    let q = ref zero in
    let r = ref a in
    while deg !r >= degb do
      let fact = (F.div (leading_coeff !r) (leading_coeff b)) *. x **^ (deg !r - degb) in 
      r := !r -^ fact *^ b;
      q := !q +^ fact
    done;
    (!q, !r)
    
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


   




