open Graphics
open Interface

let open_window
      ?(title=" Mondrian")
      ?(width=window_width)
      ?(height=window_height)
      () =
  open_graph (" " ^
                (string_of_int width) ^ "x" ^
                  (string_of_int height));

  set_window_title title;
  synchronize ();

  let page = ref Menu.menu in
  Menu.show_on_open ();

  let rec loop () =
    let e = wait_next_event [Mouse_motion; Button_down] in
    let _ =
      match !page.handler e with
      | Some p -> page := p;
      | None -> ()
    in
    loop ()
  in

  try
    loop ();
  with Exit -> ();
  close_graph ()

let _ = open_window ()
