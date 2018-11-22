open Graphics
open Bsp
open Ratio

let window_width = 600
let window_height = 600

let f_window_width = ratio_of_int window_width
let f_window_height = ratio_of_int window_height

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
      f e;
      synchronize ();
    done;
  with Exit -> ();
  close_graph ()

let draw_line l =
  Graphics.set_line_width 5;
  Graphics.set_color white;
  moveto (int_of_ratio l.pt1.x) (int_of_ratio l.pt1.y);
  lineto (int_of_ratio l.pt2.x) (int_of_ratio l.pt2.y);
  Graphics.set_line_width 3;
  Graphics.set_color black;
  moveto (int_of_ratio l.pt1.x) (int_of_ratio l.pt1.y);
  lineto (int_of_ratio l.pt2.x) (int_of_ratio l.pt2.y)

let rec draw_bsp_line bsp =
  match bsp with
  | L (l, left, right) ->
    draw_line l;
    draw_bsp_line left;
    draw_bsp_line right
  | R color -> ()

let plot_bsp bsp =
  Bsp.iter
    (fun color pts ->
      set_color color;
      let barycenter = center pts in
      let poly = Array.of_list pts in
      Array.sort (compare_counter_clockwise barycenter) poly;
      Array.iter print_point poly;
      print_endline "";
      let poly =
        Array.map
          (fun pt -> int_of_ratio pt.x, int_of_ratio pt.y) poly
      in Graphics.fill_poly poly)
    bsp f_window_width f_window_height;
  draw_bsp_line bsp;
  let _, e = edges f_window_width f_window_height
  in List.iter draw_line e
