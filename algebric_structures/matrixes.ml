open Fields
open Rings

module type MATRIXES_RING = sig
  module F : FIELD
  include RING with type t = F.t array array
  val init_matrix: (int -> int -> F.t) -> t
  val ( +^ ) : t -> t -> t
  val ( -^ ) : t -> t -> t
  val ( *^ ) : t -> t -> t
  val ( **^ ) : t -> int -> t
  val ( *. ) : F.t -> t -> t
  val transpose : t -> t
  val row_switch : t -> int -> int -> unit 
  val row_mul : ?elementary:bool -> t -> int -> F.t -> unit 
  val row_addition : ?elementary:bool -> t -> int -> F.t -> int -> unit
  val gaussian_elimination : t -> t
  val apply : t -> F.t array -> F.t array
  val of_int_matrix : int array array -> t
  val to_int_matrix : t -> int array array
  val kernel_element : t -> F.t array
  val vector_to_string : F.t array -> string
end

module type MATRIXES_PARAM = sig
  module F : FIELD
  val n : int
end

module MakeMatrixes (P : MATRIXES_PARAM): MATRIXES_RING with module F = P.F = struct
  module F = P.F 
  let n = P.n 
  type t = F.t array array

  let zero = Array.make_matrix n n F.zero
  let one = Array.init_matrix n n (fun i j -> if i = j then F.one else F.zero)

  let init_matrix f: t = Array.init_matrix n n f

  let apply (m: t) (x: F.t array): F.t array = 
    let y = Array.make n F.zero in 
    for i = 0 to n - 1 do 
      let t = ref F.zero in
      for j = 0 to n - 1 do 
        t := F.add !t (F.mul m.(i).(j) x.(i))
      done;
      y.(i) <- !t
    done;
    y

  let add: t -> t -> t = Array.map2 (Array.map2 F.add)

  let sub: t -> t -> t = Array.map2 (Array.map2 F.sub)

  let mul (a: t) (b: t): t = 
    let c = Array.copy zero in 
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let t = ref F.zero in
        for k = 0 to n - 1 do
          t := F.add !t (F.mul a.(i).(k) b.(k).(j))
        done;
        c.(i).(j) <- !t
      done
    done;
    c

  let external_mul (b: int): t -> t = Array.map (Array.map (F.external_mul b))

  let exp a n: t = Utils.fast_operation mul one a n

  let ( *. ) (b: F.t): t -> t = Array.map (Array.map (F.mul b))

  let equal: t -> t -> bool = Array.for_all2 (Array.for_all2 ( = ))

  let ( +^ ) = add
  let ( -^ ) = sub
  let ( *^ ) = mul
  let ( **^ ) = exp

  let transpose (m: t): t = init_matrix (fun i j -> m.(j).(i))

  let of_int a = external_mul a one 
  let to_int _ = failwith "Who the hell wants to convert a matrix to an int??? What are you expecting?"

  let of_int_matrix: int array array -> t = Array.map (Array.map F.of_int)

  let to_int_matrix: t -> int array array = Array.map (Array.map F.to_int)

  let row_switch (m: t) i j =
    if i = j then () else begin
      let t = m.(i) in
      m.(i) <- m.(j);
      m.(j) <- t
    end
  let row_mul ?(elementary=true) m i x =
    assert (not elementary || (not (F.equal x F.zero)));
    Array.map_inplace (F.mul x) m.(i)
  let row_addition ?(elementary=true) m i x j =
    assert (not elementary || i <> j); 
    Array.mapi_inplace (fun k a -> F.add a (F.mul x m.(j).(k))) m.(i)

  let gaussian_elimination (m: t): t =
    let cp = Array.map (Array.copy) m in 
    let r = ref (-1) in (* Last pivot index *)
    for j = 0 to n - 1 do 
      let dif0 x = not (F.equal x F.zero) in
      let k = ref None in 
      for i = !r + 1 to n - 1 do (* Could stop the loop earlier, but no break in ocaml and I'm lazy *)
        if dif0 cp.(i).(j) then begin
          k := Some i;
        end
      done;
      match !k with
      | None -> ()
      | Some k -> begin 
        incr r;
        row_mul cp k (F.inv cp.(k).(j));
        if k <> !r then row_switch cp k !r;
        for i = 0 to n - 1 do
          if i <> !r then row_addition cp i (F.sub F.zero cp.(i).(j)) !r
        done
      end
    done;
    cp

  let kernel_element (m: t): F.t array = (* ChatGPTed *)
    let rref = gaussian_elimination m in 
    let n_rows = Array.length rref in
    let n_cols = Array.length rref.(0) in 
    
    (* 1. Repérer les colonnes qui sont des pivots *)
    let pivot_cols = Array.make n_rows (-1) in
    let is_pivot_col = Array.make n_cols false in
    
    let is_zero = F.equal F.zero in 

    for i = 0 to n_rows - 1 do
      let rec find_pivot j =
        if j >= n_cols then -1
        else if not (is_zero rref.(i).(j)) then j
        else find_pivot (j + 1)
      in
      let p = find_pivot 0 in
      pivot_cols.(i) <- p;
      if p <> -1 then is_pivot_col.(p) <- true
    done;

    (* 2. Choisir la première variable libre STRICTEMENT POSITIVE (> 0) *)
    let free_var_idx = ref (-1) in
    begin
      try
        for j = 1 to n_cols - 1 do (* Démarrer à 1 pour ignorer le terme constant *)
          if not is_pivot_col.(j) then (
            free_var_idx := j;
            raise Exit
          )
        done
      with Exit -> ()
    end;

    (* 3. Si aucune variable libre > 0 n'est trouvée (le noyau est de dim 1 et contient que le constant) *)
    if !free_var_idx = -1 then 
      Array.make n_cols F.zero (* Renvoyer le vecteur nul comme demandé *)
    else begin
      (* 4. Construction du vecteur solution (polynôme non constant) *)
      let x = Array.make n_cols F.zero in
      
      (* On fixe la variable libre choisie à 1 *)
      x.(!free_var_idx) <- F.one;

      (* 5. Remontée (Back-substitution) *)
      for i = n_rows - 1 downto 0 do
        let p_col = pivot_cols.(i) in
        if p_col <> -1 then (
          let sum = ref F.zero in
          for j = p_col + 1 to n_cols - 1 do
            sum := F.add !sum (F.mul rref.(i).(j) x.(j))
          done;
          
          let pivot_val = rref.(i).(p_col) in
          x.(p_col) <- F.div (F.sub F.zero !sum) pivot_val
        )
      done;
      x
    end

  let pp_matrix fmt m =
    let open Format in
    if Array.length m = 0 then fprintf fmt "[||]"
    else (
      (* Precompute string matrix + column widths *)
      let str = Array.map (Array.map F.to_string) m in
      let cols = Array.length str.(0) in
      let widths =
        Array.init cols (fun j ->
          Array.fold_left (fun acc row -> max acc (String.length row.(j))) 0 str
        )
      in
      fprintf fmt "@[<v>";
      Array.iter (fun row ->
        fprintf fmt "[| ";
        Array.iteri (fun j x ->
          let w = widths.(j) in
          fprintf fmt "%*s" w x;
          if j < cols - 1 then fprintf fmt " ; "
        ) row;
        fprintf fmt " |]@,"
      ) str;
      fprintf fmt "@]"
    )

  let to_string m =
    Format.asprintf "%a" pp_matrix m

  let vector_to_string (v : F.t array) : string =
    let lines =
      Array.to_list (Array.map F.to_string v)
    in
    String.concat "\n" lines
end