open Graphics
open Interface
open Bsp_view
open Settings

let min_area = 1000
let max_area = 16000

let max_black = 50
let min_black = 0

let game_mode = ref Classic
let area = ref ((max_area + min_area) / 2)
let black_probability = ref ((max_black + min_black) / 2)
let color_mode = ref RGColor

let game_mode_buttons, show_game_mode_buttons =
  buttons_groups
    [
      create_button "Classic mode" 180. 350. 110 75, (fun () -> game_mode := Classic);
      create_button "Extrem mode" 310. 350. 110 75, (fun () -> game_mode := Extrem);
    ]

let game_color_buttons, show_game_color_buttons =
  buttons_groups
    [
      create_button "2 Colors" 180. 450. 110 75, (fun () -> color_mode := RGColor);
      create_button "3 Colors" 310. 450. 110 75, (fun () -> color_mode := RGBColor);
    ]

let buttons =
  [
    create_button "Play" 175. 75. 250 75, fun () -> ()
  ]

let slide_bar_width = 200
let slide_bar_x = (!window_width - slide_bar_width) / 2
let slide_bar_xx = slide_bar_x + slide_bar_width
let slide_bar_height = 30
let slide_bar_padding = 10

let slide_bar title y min max value st_opt =
  set_color white;
  fill_poly
    [|(slide_bar_x, y - (slide_bar_height / 2));
      (slide_bar_xx, y - (slide_bar_height / 2));
      (slide_bar_xx, y + (slide_bar_height / 2));
      (slide_bar_x, y + (slide_bar_height / 2))|];

  set_color black;
  set_line_width 2;
  moveto slide_bar_x y;
  lineto slide_bar_xx y;
  set_line_width 1;
  moveto slide_bar_x (y - 10);
  lineto slide_bar_x (y + 10);
  moveto slide_bar_xx (y - 10);
  lineto slide_bar_xx (y + 10);

  let _ =
    match st_opt with
    | None -> ()
    | Some st ->
       if st.button && st.mouse_x >= slide_bar_x && st.mouse_x <= slide_bar_xx &&
            st.mouse_y >= y - slide_bar_padding && st.mouse_y <= y + slide_bar_padding
       then value := (st.mouse_x - slide_bar_x) * (max - min) /
                       slide_bar_width + min;
  in

  set_line_width 2;
  let x = (!value - min) * slide_bar_width /
            (max - min) + slide_bar_x
  in
  moveto x (y - 10);
  lineto x (y + 10);

  let w, _ = text_size title in
  moveto ((slide_bar_width - w) / 2 + slide_bar_x) (y + slide_bar_height / 2);
  draw_string title;

  moveto slide_bar_x (y - slide_bar_height / 2);
  draw_string (string_of_int min);
  moveto slide_bar_xx (y - slide_bar_height / 2);
  draw_string (string_of_int max);
  moveto (slide_bar_x + slide_bar_width / 2) (y - slide_bar_height / 2);
  draw_string (string_of_int !value)

let show_title () =
  let title = "Mondrian" in
  let w, _ = text_size title in
  let x_title = (!window_width - w) / 2 in
  let y_title = 600 in

  set_color black;
  moveto x_title y_title;
  draw_string title

let show_buttons st_opt =
  let hover_predicate =
    match st_opt with
    | Some st -> fun btn -> is_click btn st
    | _ -> fun _ -> false
  in
  List.iter
    (fun (btn, _) -> print_btn ~hover:(hover_predicate btn) btn)
    buttons

let show_on_open () =
  show_title ();
  slide_bar "Minimum area" 200 min_area max_area area None;
  slide_bar "Black probability" 275 min_black max_black black_probability None;
  show_game_mode_buttons ();
  show_game_color_buttons ();
  show_buttons None

let show_menu st =
  show_title ();
  slide_bar "Minimum area" 200 min_area max_area area (Some st);
  slide_bar "Black probability" 275 min_black max_black black_probability (Some st);
  show_game_mode_buttons ();
  show_game_color_buttons ();
  show_buttons (Some st)

let create_page _ =
  let module B  =
    struct
      let mode = !game_mode
      let color = !color_mode
      let min_area = !area
      let black_probability = !black_probability
    end
  in
  Some (make_game_view (module B))

let select_mode st =
  show_menu st;
  if not st.button
  then None
  else
    let rec home_made_find_opt p l =
      match l with
      | h :: q -> if p h
                  then Some h
                  else home_made_find_opt p q
      | [] -> None
    in
    let hovers =
      home_made_find_opt
        (fun (btn, _) -> is_click btn st)
        (buttons @ game_mode_buttons @ game_color_buttons)
    in
    match hovers with
    | None -> None
    | Some (btn, _) when btn.text = "Play" -> create_page st
    | Some (_, hdl) -> hdl (); None

let menu =
  {
    plot = show_menu;
    handler = select_mode;
  }
