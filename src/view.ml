open Graphics
open Geometry
open Bsp

let window_width = 600
let window_height = 600

let f_window_width = float_of_int window_width
let f_window_height = float_of_int window_height

exception Exit

let do_with_window
      ?(title=" Mondrian")
      ?(width=window_width)
      ?(height=window_height)
      ?(on_open=ignore)
      f =
  open_graph (" " ^
                (string_of_int width) ^ "x" ^
                  (string_of_int height));

  set_window_title title;

  on_open ();
  synchronize ();

  try
    while true do
      let e = wait_next_event [Button_down; Key_pressed] in
      if e.keypressed && e.key = 'q' then raise Exit;
      clear_graph ();
      f e;
      synchronize ();
    done;
  with Exit -> ();
  close_graph ()

(* let plot_bsp (type s) (module B : Bsp_type with type bsp = s) bsp =
 *   B.iter_area
 *     (fun color pts ->
 *       set_color color;
 *       let poly =
 *         Array.map
 *           (fun pt -> int_of_float pt.x, int_of_float pt.y) (Array.of_list pts)
 *       in Graphics.fill_poly poly)
 *     bsp f_window_width f_window_height;
 *   B.iter_line draw_line bsp *)

