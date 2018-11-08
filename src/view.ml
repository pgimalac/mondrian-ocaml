open Graphics
open Bsp

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

  f ();
  synchronize ();
  
  ignore (wait_next_event [Button_down]);  
  close_graph ()

let plot_on_click e =
  set_color black;
  plot e.mouse_x e.mouse_y    

let rec plot_bsp bsp =
  match bsp with
  | L (l, left, right) ->
     let _ =
       match l.c with
       | Red -> set_color red
       | Blue -> set_color blue
     in
     moveto (int_of_float l.pt1.x) (int_of_float l.pt1.y);
     lineto (int_of_float l.pt2.x) (int_of_float l.pt2.y);
     plot_bsp left;
     plot_bsp right
  | R c -> ()
     
