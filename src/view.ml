open Graphics

let window_width = 600
let window_height = 600

let do_with_window
      ?(title=" Mondrian")
      ?(width=window_width)
      ?(height=window_height)
      f =
  open_graph (" " ^
                (string_of_int width) ^ "x" ^
                  (string_of_int height));

  set_window_title title;
  
  while true do
    let e = wait_next_event [Button_down; Key_pressed] in
    f e;
    synchronize ();
  done;

  close_graph ()

let plot_on_click e =
  set_color black;
  plot e.mouse_x e.mouse_y    
