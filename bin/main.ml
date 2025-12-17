(**********************************************************************)
(* Interface graphique BCH – version CORRIGÉE, syntaxiquement saine   *)
(* Conforme exactement à l'exemple fourni (lablgtk3)                 *)
(**********************************************************************)

let () =
  ignore @@ GMain.init ();

  (* Fenêtre principale *)
  let w = GWindow.window ~width:900 ~height:500 ~title:"BCH – Simulation" () in
  w#connect#destroy ~callback:GMain.quit |> ignore;

  (* VBox principale *)
  let main_box = GPack.vbox ~spacing:14 ~packing:w#add () in


  (********************************************************************)
  (* Zone du haut                                                    *)
  (********************************************************************)

  let top_box = GPack.vbox ~spacing:6 ~packing:main_box#pack () in

  (* Entrée message (SEULE entrée éditable) *)
  let entry_message = GEdit.entry ~packing:top_box#pack () in
  entry_message#set_placeholder_text "Message à envoyer";
  entry_message#misc#modify_font_by_name "Sans 16";


  (* Ligne : choix du code + bouton *)
  let code_box = GPack.hbox ~spacing:6 ~packing:top_box#pack () in

  let code_combo = GEdit.combo_box_text ~packing:code_box#pack () in
  let add_list b = List.iter (GEdit.text_combo_add b) in
  add_list code_combo [ "BCH(7,4)"; "BCH(15,11)" ];
  let code_cb, _ = code_combo in
  

  let send_button = GButton.button ~label:"Envoyer" ~packing:code_box#pack () in

  (********************************************************************)
  (* Zone du bas : résultats                                         *)
  (********************************************************************)

  let result_box = GPack.vbox ~spacing:6 ~packing:main_box#pack () in

  (* Fonction utilitaire : une ligne label-titre + label-valeur *)
  let make_row title =
    let h = GPack.hbox ~spacing:6 ~packing:result_box#pack () in
    let _title = GMisc.label ~text:title ~width:180 ~packing:h#pack () in
    _title#misc#modify_font_by_name "Sans 14";
    let value = GMisc.label ~text:"" ~xalign:0.0 ~packing:h#pack () in
    value#misc#modify_font_by_name "Sans 14";
    value
  in

  let label_encoded = make_row "Message encodé" in

  (* Canal : combobox *)
  let h_canal = GPack.hbox ~spacing:6 ~packing:result_box#pack () in
  let label_canal =
    GMisc.label ~text:"Canal" ~width:180 ~packing:h_canal#pack ()
  in
  label_canal#misc#modify_font_by_name "Sans 14";
  let canal_combo = GEdit.combo_box_text ~packing:h_canal#pack () in
  add_list canal_combo [ "Canal symétrique"; "Canal bruité" ];
  let canal_cb, _ = canal_combo in
  code_cb#misc#modify_font_by_name "Sans 14";
  canal_cb#misc#modify_font_by_name "Sans 14";
  send_button#misc#modify_font_by_name "Sans 14";


  let label_with_error = make_row "Encodé avec erreur" in
  let label_corrected = make_row "Encodé corrigé" in
  let label_decoded = make_row "Message décodé" in

  (********************************************************************)
  (* Callback bouton Envoyer                                          *)
  (********************************************************************)

  ignore
    (send_button#connect#clicked ~callback:(fun _ ->
         let message = entry_message#text in
         let code_index = code_cb#active in
         let canal_index = canal_cb#active in

         (* TODO : remplacer par tes vraies fonctions BCH *)
         let encoded = Printf.sprintf "ENC[%d](%s)" code_index message in
         let with_error = Printf.sprintf "%s + err(%d)" encoded canal_index in
         let corrected = Printf.sprintf "CORR(%s)" with_error in
         let decoded = Printf.sprintf "DEC(%s)" corrected in

         label_encoded#set_text encoded;
         label_with_error#set_text with_error;
         label_corrected#set_text corrected;
         label_decoded#set_text decoded;

         flush stdout));

  w#show ();
  GMain.main ()