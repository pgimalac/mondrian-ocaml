open Graphics
open Bsp

let window_width = 600
let window_height = 600

exception Break
                  
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
      if e.keypressed && e.key = 'q' then raise Break;
      f e;
      synchronize ();
    done;
  with Break -> ();
  close_graph ()

let plot_on_click e =
  set_color black;
  plot e.mouse_x e.mouse_y    

let set_color c =
  match c with
  | Red -> set_color red
  | Blue -> set_color blue
         
let edges =
  let w = float_of_int window_width in
  let h = float_of_int window_height in
  let e1, e2, e3, e4 =
    {x=0.; y=0.}, {x=w; y=0.}, {x=0.; y=h}, {x=w; y=h}
  in [e1; e2; e3; e4], [
      {pt1=e1;pt2=e2;c=Blue};
      {pt1=e1;pt2=e3;c=Blue};
      {pt1=e2;pt2=e4;c=Blue};
      {pt1=e3;pt2=e4;c=Blue}
    ] 
  
let plot_bsp bsp =
  let rec aux bsp pts edges =
    match bsp with
    | L (l, left, right) ->
       let pts, edges_l, edges_r =
         List.fold_left
           (fun (pts, el, er) line ->
             match intersect line l with
             | None ->
                if is_left line.pt1 l
                then pts, line :: el, er
                else pts, el, line :: er
             | Some (pt) ->
                if not (List.mem pt pts)
                then pt :: pts, line :: el, line :: er
                else pts, line :: el, line :: er
           ) (pts, [], []) edges
       in
       set_color l.c;
       moveto (int_of_float l.pt1.x) (int_of_float l.pt1.y);
       lineto (int_of_float l.pt2.x) (int_of_float l.pt2.y);
       let left_pts, right_pts =
         List.fold_left (fun (pts_l, pts_r) pt ->
             if is_left pt l
             then if is_right pt l
                  then pt :: pts_l, pt :: pts_r
                  else pt :: pts_l, pts_r
             else if is_right pt l
             then pts_l, pt :: pts_r
             else pts_l, pts_r) ([], []) pts
       in
       aux left left_pts (l :: edges_l);
       aux right right_pts (l :: edges_r)
    | R c ->
       match c with
       | None -> ()
       | Some c ->
          set_color c;
          let poly =
            Array.map
              (fun pt -> int_of_float pt.x, int_of_float pt.y)
              (Array.of_list pts) in
          Graphics.fill_poly poly;
          Array.iter (fun (x, y) -> Graphics.fill_circle x y 10) poly
  in
  let pts, lines = edges in
  aux bsp pts lines
     
