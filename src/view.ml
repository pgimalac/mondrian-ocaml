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
      f e;
      synchronize ();
    done;
  with Exit -> ();
  close_graph ()

type button =
  {
    text : string;
    pt   : point;
    w    : int;
    h    : int;
  }
  
let print_btn ?(color_bkg=green) ?(color_fg=white) btn =
  let x, y = int_of_float btn.pt.x, int_of_float btn.pt.y in
  set_color color_bkg;
  fill_rect x y btn.w btn.h;
  set_color color_fg;
  moveto x y;
  draw_string btn.text;
  ()

let is_click b st =
  let bx, by = int_of_float b.pt.x, int_of_float b.pt.y in
  st.mouse_x >= bx && st.mouse_x <= bx + b.w &&
    st.mouse_y >= by && st.mouse_y <= by + b.h
  
type game_mode = Classic | Extrem
  
let menu st =
  let btn_classic = {
      text = "Classic mode";
      pt = {x = 150.;y = 325.};
      w = 250;
      h = 75} in
  let btn_extrem = {
      text = "Extrem mode";
      pt = {x = 150.;y = 200.};
      w = 250;
      h = 75} in
  let print_menu =
    print_btn btn_classic;
    print_btn btn_extrem;
  in
  match st with
  | Some st when st.button ->
     if is_click btn_classic st
     then Some Classic
     else if is_click btn_extrem st
     then Some Extrem
     else None
  | _ -> print_menu;
        None

module Make (B : Bsp_type) = struct

  let bsp = ref None
  
  let plot_bsp bsp =
    print_endline "plot";
    B.iter_area
      (fun color pts ->
        set_color color;
        let poly =
          Array.map
            (fun pt -> int_of_float pt.x, int_of_float pt.y) (Array.of_list pts)
        in fill_poly poly)
      bsp f_window_width f_window_height;
    B.iter_line (draw_line white 5) bsp f_window_width f_window_height;
    B.iter_line (draw_line black 3) bsp f_window_width f_window_height

  let plot () =
    match !bsp with
    | None -> failwith "No bsp"
    | Some b -> plot_bsp b
    
  let view () =
    bsp := Some (B.generate_random_bsp f_window_width f_window_height);
    let handler e =
      match !bsp with
      | None -> failwith "No bsp"
      | Some b ->
         if e.button
         then begin
             bsp := Some (B.change_color b {
                              x = float_of_int e.mouse_x;
                              y = float_of_int e.mouse_y});
           end;
         plot_bsp b
    in handler

end
