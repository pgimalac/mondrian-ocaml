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
      let e = wait_next_event [Button_down; Key_pressed; Mouse_motion] in
      if e.keypressed && e.key = 'q' then raise Exit;
      f e;
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
  let w, h = text_size btn.text in
  moveto ((int_of_float btn.pt.x) + (btn.w - w) / 2)
    ((int_of_float btn.pt.y) + (btn.h - h) / 2);
  draw_string btn.text;
  ()

let is_click b st =
  let bx, by = int_of_float b.pt.x, int_of_float b.pt.y in
  st.mouse_x >= bx && st.mouse_x <= bx + b.w &&
    st.mouse_y >= by && st.mouse_y <= by + b.h

type game_mode = Classic | Extrem

let menu st =
  let title = "Mondrian" in
  let w, h = text_size title in
  let x_title = (window_width / 2) - w / 2 in
  let y_title = 450 in

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
  let print_menu ?(classic=false) ?(extrem=false) () =
    set_color black;
    moveto x_title y_title;
    draw_string title;

    if classic
    then print_btn ~color_bkg:red btn_classic
    else print_btn btn_classic;

    if extrem
    then print_btn ~color_bkg:red btn_extrem
    else print_btn btn_extrem;
  in
  match st with
  | Some st when st.button ->
     if is_click btn_classic st
     then Some Classic
     else if is_click btn_extrem st
     then Some Extrem
     else None
  | Some st when is_click btn_classic st ->
     print_menu ~classic:true ();
     None
  | Some st when is_click btn_extrem st ->
     print_menu ~extrem:true ();
     None
  | _ ->
     print_menu ();
     None

module type Bsp_view = sig

  val plot : unit -> unit

  val view : unit -> Graphics.status -> unit

end

module Make (B : Bsp_type) : Bsp_view = struct

  let bsp = ref (B.generate_random_bsp f_window_width f_window_height)

  let plot_bsp bsp =
    B.iter_area
      (fun color pts ->
        set_color color;
        let poly =
          Array.map
            (fun pt -> int_of_float pt.x, int_of_float pt.y) (Array.of_list pts)
        in fill_poly poly)
      bsp f_window_width f_window_height;
    B.iter_line (draw_line white 5) bsp f_window_width f_window_height;
    B.iter_line (draw_line black 3) bsp f_window_width f_window_height;
    let _, e = edges f_window_width f_window_height in
    List.iter (fun x -> draw_line white 5 x; draw_line black 3 x) e

  let plot () = plot_bsp !bsp

  let view () =
    let handler e =
      if e.button
      then begin
          bsp := B.change_color !bsp {
                     x = float_of_int e.mouse_x;
                     y = float_of_int e.mouse_y};
          plot_bsp !bsp
        end
    in handler

end
