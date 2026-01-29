type image = {
  width : int;
  height : int;
  data : int array; (* Tableau linéaire de 0 et 1 *)
}

(* Création d'une image test *)
(* mieux qu'une image vide pour voir les erreurs *)
let create_pattern w h =
  let data = Array.init (w * h) (fun i ->
    let x = i mod w and y = i / w in
    (* Motif : Un cercle au centre + un quadrillage *)
    let cx, cy = w / 2, h / 2 in
    let r = min w h / 3 in
    let in_circle = (x - cx)*(x - cx) + (y - cy)*(y - cy) < r*r in
    let grid = (x / 10) mod 2 = (y / 10) mod 2 in
    if in_circle && not grid then 1 else 0
  ) in
  { width = w; height = h; data }

(* Sauvegarde au format P1 *)
let save (img : image) (filename : string) =
  let oc = open_out filename in
  Printf.fprintf oc "P1\n%d %d\n" img.width img.height;
  (* Le format P1 n'aime pas les lignes trop longues, on coupe tous les 70 cars *)
  Array.iteri (fun i bit ->
    Printf.fprintf oc "%d " bit;
    if (i + 1) mod 35 = 0 then Printf.fprintf oc "\n"
  ) img.data;
  close_out oc

(* Conversion utilitaire *)
let to_channel_input img = img.data

let of_channel_output w h data = { width = w; height = h; data }