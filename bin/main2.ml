let () =
  ignore @@ GMain.init ();
  let w = GWindow.window ~width:320 ~height:240 ~title:"Lablgtk3" () in
  let box = GPack.vbox ~packing:w#add () in
  let button_click_me =
    GButton.button ~label:"Click me!" ~packing:box#pack ()
  in
  let button_dont_click =
    GButton.button ~label:"Don't click me!" ~packing:box#pack ()
  in
  let entry = GEdit.entry ~packing:box#pack () in
  let text_combot = GEdit.combo_box_text ~packing:box#pack () in
  let add_list b = List.iter (GEdit.text_combo_add b) in
  add_list text_combot [ "salut"; "comment"; "ça"; "va" ];
  let combot_box, _ = text_combot in
  (* combot_box# *)

  let label = GMisc.label ~packing:box#pack () in 

  ignore
    (button_click_me#connect#clicked ~callback:(fun _ ->
         Printf.printf "Yeah! =) %s\n" entry#text;
         label#set_text entry#text;
         flush stdout));
  ignore
    (button_dont_click#connect#clicked ~callback:(fun _ ->
         Printf.printf "No! ='(, %d\n" combot_box#active (* Indice du choix actif *);
         flush stdout));

  w#show ();
  GMain.main ()
