open Graphics
open Bsp

let window_width = 600
let window_height = 600

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

let center pts =
  let l, sx, sy =
    List.fold_left
      (fun (l, sx, sy) pt -> l +. 1., pt.x +. sx, pt.y +. sy)
      (0., 0., 0.) pts
  in {x = sx /. l; y = sy /. l}
              
let compare_counter_clockwise center x y =
  let f =
    if (x.x -. center.x) *. (y.x -. center.x) >= 0.
    then (x.x -. center.x) *. (x.y -. y.y)
    else x.x -. y.x
  in int_of_float f

let edges =
  let w = float_of_int window_width in
  let h = float_of_int window_height in
  let e1, e2, e3, e4 =
    {x=0.; y=0.}, {x=w; y=0.}, {x=0.; y=h}, {x=w; y=h}
  in [e1; e2; e3; e4], [
      {pt1=e1;pt2=e2};
      {pt1=e1;pt2=e3};
      {pt1=e2;pt2=e4};
      {pt1=e3;pt2=e4}
    ] 

let plot_bsp bsp =
  let rec aux bsp pts edges =
    match bsp with
    | L (l, left, right) ->
       let pts, edges_l, edges_r =
         List.fold_left
           (fun (acc, el, er) line ->
             match intersect line l with
             | None ->
                if is_left line.pt1 l
                then acc, line :: el, er
                else acc, el, line :: er
             | Some (pt) ->
                let add =
                  not (List.mem pt acc) &&
                    pt.x >= min l.pt1.x l.pt2.x &&
                      pt.x <= max l.pt1.x l.pt2.x &&
                    pt.y >= min l.pt1.y l.pt2.y &&
                      pt.y <= max l.pt1.y l.pt2.y
                in
                if add
                then pt :: acc, line :: el, line :: er
                else acc, line :: el, line :: er
           ) (pts, [], []) edges
       in
       let left_pts, right_pts =
         List.fold_left (fun (pts_l, pts_r) pt ->
             if is_left pt l
             then if is_right pt l
                  then pt :: pts_l, pt :: pts_r
                  else pt :: pts_l, pts_r
             else pts_l, pt :: pts_r
           ) ([], []) pts
       in
       l :: aux left left_pts (l :: edges_l) @
       aux right right_pts (l :: edges_r)
    | R color ->
       match color with
       | None -> []
       | Some color as c ->
          set_color c;
          let barycenter = center pts in
          let poly = Array.of_list pts in
          Array.sort (compare_counter_clockwise barycenter) poly;
          let poly =
            Array.map
              (fun pt -> int_of_float pt.x, int_of_float pt.y) poly in
          Graphics.fill_poly poly;
          []
  in
  let pts, lines = edges in
  let lines = aux bsp pts lines in
  Graphics.set_line_width 2;
  List.iter (fun l -> 
      Graphics.set_color black;
      moveto (int_of_float l.pt1.x) (int_of_float l.pt1.y);
      lineto (int_of_float l.pt2.x) (int_of_float l.pt2.y))
    lines
  
     
