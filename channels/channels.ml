type data = int array

let flip bit = 1 - bit

(* ======================================================================== *)
(* CANAL SYMÉTRIQUE BINAIRE (BSC - Binary Symmetric Channel)                *)
(* C'est le bruit "blanc" classique. Chaque bit a une probabilité p         *)
(* d'être inversé, indépendamment des autres.                               *)
(* ======================================================================== *)
let bsc (p : float) (input : data) : data =
  Array.map (fun bit -> if Random.float 1.0 < p then flip bit else bit) input

(* ======================================================================== *)
(* MODÈLE DE GILBERT-ELLIOTT (Canal à Mémoire / Bouffées d'erreurs)         *)
(* Modélisé par une chaîne de Markov à deux états :                         *)
(* - G (Good) : État calme, peu d'erreurs.                                  *)
(* - B (Bad)  : État bruité, beaucoup d'erreurs (la "bouffée").             *)
(* *)
(* Paramètres :                                                             *)
(* - p_gb : Probabilité de transition Good -> Bad (début de bouffée)        *)
(* - p_bg : Probabilité de transition Bad -> Good (fin de bouffée)          *)
(* - err_g : Probabilité d'erreur quand on est dans l'état Good (souvent 0) *)
(* - err_b : Probabilité d'erreur quand on est dans l'état Bad (ex: 0.5)    *)
(* ======================================================================== *)

type state = Good | Bad

let gilbert_elliott ~p_gb ~p_bg ~err_g ~err_b (input : data) : data =
  let current_state = ref Good in

  Array.map
    (fun bit ->
      let current_error_prob =
        match !current_state with Good -> err_g | Bad -> err_b
      in

      let output_bit =
        if Random.float 1.0 < current_error_prob then flip bit else bit
      in 

      begin current_state := 
        match !current_state with
        | Good -> if Random.float 1.0 < p_gb then Bad else Good
        | Bad -> if Random.float 1.0 < p_bg then Good else Bad
      end;

      output_bit)
    input

(* ======================================================================== *)
(* CANAL À EFFACEMENT (Erasure Channel) simulé par remplissage à 0          *)
(* Simule une perte de signal où le récepteur lit '0' par défaut.           *)
(* Ce n'est pas une inversion, c'est un écrasement.                         *)
(* ======================================================================== *)
let erasure (p : float) (input : data) : data =
  Array.map (fun bit -> if Random.float 1.0 < p then 0 else bit) input

(* ======================================================================== *)
(* OUTILS D'ANALYSE                                                         *)
(* Pour visualiser les erreurs introduites (différence entrée/sortie)       *)
(* ======================================================================== *)

(* Renvoie une chaîne montrant les erreurs : '.' = ok, 'X' = erreur *)
let visualize_diff input output =
  let n = Array.length input in
  let s = Bytes.create n in
  for i = 0 to n - 1 do
    if input.(i) = output.(i) then Bytes.set s i '.' else Bytes.set s i 'X'
  done;
  Bytes.to_string s

(* Compte le nombre d'erreurs *)
let count_errors input output =
  let err = ref 0 in
  Array.iter2 (fun a b -> if a <> b then incr err) input output;
  !err
