open Graphics
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
      f e;
      synchronize ();
    done;
  with Exit -> ();
  close_graph ()

let set_color c =
  match c with
  | None -> set_color black
  | Some Red -> set_color red
  | Some Blue -> set_color blue

let draw_line l =
  Graphics.set_line_width 5;
  Graphics.set_color white;
  moveto (int_of_float l.pt1.x) (int_of_float l.pt1.y);
  lineto (int_of_float l.pt2.x) (int_of_float l.pt2.y);
  Graphics.set_line_width 3;
  Graphics.set_color black;
  moveto (int_of_float l.pt1.x) (int_of_float l.pt1.y);
  lineto (int_of_float l.pt2.x) (int_of_float l.pt2.y)

let rec draw_bsp_line bsp =
  match bsp with
  | L (l, left, right) ->
    draw_line l;
    draw_bsp_line left;
    draw_bsp_line right
  | R color -> ()

let plot_bsp bsp =
  let rec fill_bsp bsp pts =
    match bsp with
    | L (l, left, right) ->
       let left_pts, right_pts =
         List.fold_left (fun (pts_l, pts_r) pt ->
             if is_left pt l
             then if is_right pt l
                  then pt :: pts_l, pt :: pts_r
                  else pt :: pts_l, pts_r
             else pts_l, pt :: pts_r
           ) ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts
       in
       fill_bsp left left_pts;
       fill_bsp right right_pts
    | R color ->
       match color with
       | None -> ()
       | c ->
          set_color c;
          let barycenter = center pts in
          let poly = Array.of_list pts in
          Array.sort (compare_counter_clockwise barycenter) poly;
          let poly =
            Array.map
              (fun pt -> int_of_float pt.x, int_of_float pt.y) poly
          in Graphics.fill_poly poly
  in
  let pts, lines = edges f_window_width f_window_height in
  fill_bsp bsp pts;
  draw_bsp_line bsp;
  List.iter draw_line lines


