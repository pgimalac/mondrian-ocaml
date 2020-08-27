open Graphics
open Interface

let open_window ?(title = " Mondrian") ?(width = window_width)
    ?(height = window_height) () =
  open_graph (" " ^ string_of_int !width ^ "x" ^ string_of_int !height);

  set_window_title title;
  auto_synchronize false;

  let page = ref Menu.menu in
  Bsp_view.menu_page := Some Menu.menu;
  Menu.show_on_open ();
  synchronize ();

  let rec loop e =
    !page.plot e;
    synchronize ();
    let e = wait_next_event [ Mouse_motion; Button_down ] in
    let _ =
      clear_graph ();
      match !page.handler e with Some p -> page := p | None -> ()
    in
    loop e
  in

  try
    let e = wait_next_event [ Mouse_motion; Button_down ] in
    loop e
  with Exit ->
    ();
    close_graph ()

let _ =
  ( if Array.length Sys.argv >= 3 then
    let home_made_int_of_string_opt s =
      try Some (int_of_string s) with Failure _ -> None
    in
    let x = home_made_int_of_string_opt Sys.argv.(1) in
    let y = home_made_int_of_string_opt Sys.argv.(2) in
    match (x, y) with
    | Some x, Some y ->
        if x >= 600 && y >= 600 then (
          window_width := x;
          window_height := y )
    | _, _ -> () );
  open_window ()
