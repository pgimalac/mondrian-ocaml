open Graphics
open Geometry

let menu_height = 100
let window_width = ref 600
let window_height = ref 600

let board_width_i () = !window_width
let board_height_i () = !window_height - menu_height

let board_width () = board_width_i () |> float_of_int
let board_height () = board_height_i () |> float_of_int

exception Exit

type button =
  {
    text      : string;
    pt        : point;
    w         : int;
    h         : int;
    bkg       : color;
    bkg_hover : color;
    fg        : color;
    fg_hover  : color;
  }

let print_btn ?(hover=false) btn =
  let x, y = int_of_float btn.pt.x, int_of_float btn.pt.y in
  set_color black;
  set_line_width 3;
  draw_poly [|x - 1, y - 1;
              x + btn.w + 1, y - 1;
              x + btn.w + 1, y + btn.h + 1;
              x - 1, y + btn.h + 1|];

  set_color (if hover then btn.bkg_hover else btn.bkg);
  fill_rect x y btn.w btn.h;

  set_color (if hover then btn.fg_hover else btn.fg);
  let w, h = text_size btn.text in
  moveto
    ((int_of_float btn.pt.x) + (btn.w - w) / 2)
    ((int_of_float btn.pt.y) + (btn.h - h) / 2);
  draw_string btn.text

let create_button
      ?(bkg=green) ?(bkg_hover=red)
      ?(fg=white) ?(fg_hover=white)
      text x y w h =
  {
    text = text;
    pt = {x = x; y = y};
    w = w;
    h = h;
    bkg = bkg;
    bkg_hover = bkg_hover;
    fg = fg;
    fg_hover = fg_hover;
  }

let is_click b st =
  let bx, by = int_of_float b.pt.x, int_of_float b.pt.y in
  st.mouse_x >= bx && st.mouse_x <= bx + b.w &&
    st.mouse_y >= by && st.mouse_y <= by + b.h

type page =
  {
    plot    : status -> unit;
    handler : status -> page option
  }

let show_buttons st buttons =
  List.iter
    (fun (btn, _) -> print_btn ~hover:(is_click btn st) btn)
    buttons

let buttons_groups buttons =
  let hd, _ = List.hd buttons in
  let selected = ref hd in
  let buttons =
    List.fold_right
      (fun (btn, hdl) btns ->
        let new_handler () =
          selected := btn;
          hdl ()
        in
        (btn, new_handler) :: btns)
    buttons []
  in
  let show_button_group () =
    List.iter (fun (btn, _) -> print_btn ~hover:(!selected=btn) btn) buttons
  in
  buttons, show_button_group
