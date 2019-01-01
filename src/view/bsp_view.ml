open Geometry
open Graphics
open Bsp
open Interface

let menu_page = ref None

let gap = 5
let wrap_line_width = 5
let colored_line_width = 3

let win_text = "Congratulation !"

let back_to_menu_button, back_to_menu_handler =
  create_button "Back to menu" 200. 250. 200 75,
  fun () -> match !menu_page with
         | None -> raise Exit
         | _ -> !menu_page

let exit_button = create_button "Exit" 200. 150. 200 75

let show_win_page st =
  set_color black;
  let w, _ = text_size win_text in
  moveto ((!window_width - w) / 2) (!window_height * 3 / 4);
  draw_string win_text;
  print_btn ~hover:(is_click back_to_menu_button st) back_to_menu_button;
  print_btn ~hover:(is_click exit_button st) exit_button

let win_page =
  {
    plot = show_win_page;
    handler =
      fun st ->
      if st.button
      then if is_click back_to_menu_button st
           then back_to_menu_handler ()
           else if is_click exit_button st
           then raise Exit
           else None
      else None
  }

module type Bsp_view = sig

  val plot : status -> unit

  val view : unit -> status -> page option

end

module Make
         (S : Settings.Game_settings)
         (C : Settings.Colors)
         (B : Bsp_complete) : Bsp_view = struct

  let bsp, nb_lines =
    let b, nb = B.generate_random_bsp (board_width ()) (board_height ()) (-1) S.min_area in
    ref b, nb

  let adjacency = ref [| |]
  let history = ref []

  let plot_bsp bsp =
    B.iter_area
      (fun label pts ->
        set_color label.region_color;
        let poly =
          Array.map
            (fun pt -> int_of_float pt.x, int_of_float pt.y)
            (Array.of_list pts)
        in
        fill_poly poly)
      bsp (board_width ()) (board_height ());
    B.iter_line
      (fun l -> draw_line black wrap_line_width l.section;
             draw_line l.line_color colored_line_width l.section)
      bsp (board_width ()) (board_height ());
    let _, e = edges (board_width ()) (board_height ()) in
    List.iter
      (fun x ->
        draw_line black wrap_line_width x;
        draw_line black colored_line_width x) e

  let count (line: line_label) colors =
    List.fold_left
      (fun (s, r, b, g) id ->
        if colors.(id) = red
        then s + 1, r + 1, b, g
        else if colors.(id) = blue
        then s + 1, r, b + 1, g
        else s + 1, r, b, g + 1)
      (0, 0, 0, 0)
      !adjacency.(line.line_id)

  let is_win () =
    B.is_solution (board_width ()) (board_height ()) !adjacency !bsp

  let w = 100
  let h = 50
  let y = board_height () +. 25.
  let x_margin = 10.
  let x_pos i = x_margin +. ((2. *. x_margin) +. (float_of_int w)) *. (float_of_int i)

  let quit_btn i = create_button "Quit" (x_pos i) y w h ~bkg:blue ~bkg_hover:red
  let clean_btn i = create_button "Clean" (x_pos i) y w h ~bkg:blue ~bkg_hover:red
  let cancel_btn i = create_button "Cancel" (x_pos i) y w h ~bkg:blue ~bkg_hover:red
  let help_btn i = create_button "Help" (x_pos i) y w h ~bkg:blue ~bkg_hover:red
  let sol_btn i = create_button "Exists solution ?" (x_pos i) y w h ~bkg:blue ~bkg_hover:red

  let no_history_msg = "No history"

  let no_solution_msg = "No solution"

  let has_solution_msg = "There is a solution"

  let text = ref ""

  let help_hdl () =
    let x = B.get_clue (board_width ()) (board_height ()) !adjacency !bsp in
    let _ = match x with
      | None -> text := no_solution_msg;
      | Some (n, c) -> begin
          match C.index c, B.find_center !bsp (board_width ()) (board_height ()) n with
          | _, None -> failwith "Help : incorrect zone number"
          | None, _ -> failwith "Help : incorrect color"
          | Some i, _ when i < 1 -> failwith "Help : incorrect color"
          | Some i, Some pt ->
             history := (pt, i) :: !history;
             bsp := B.color_nth (board_width ()) (board_height ()) !bsp n c
        end
    in
    if is_win ()
    then Some win_page
    else None

  let sol_hdl () =
    text :=
      if B.has_solution (board_width ()) (board_height ()) !adjacency !bsp
      then has_solution_msg
      else no_solution_msg;
    None

  let quit_hdl () =
    match !menu_page with
    | None -> raise Exit
    | _ -> !menu_page

  let clean_hdl () =
    history := [];
    bsp := B.clean (board_width ()) (board_height ()) !bsp;
    None

  let cancel_hdl () =
    let _ = match !history with
      | [] ->
         text := no_history_msg;
      | (pt, n) :: tl ->
         for _ = 1 to n do
           bsp := B.change_color ~reverse:true !bsp pt
         done;
         history := tl;
    in
    None

  let interface_button =
    List.mapi (fun i (btn, h) -> (btn i, h))
      [quit_btn, quit_hdl;
       clean_btn, clean_hdl;
       cancel_btn, cancel_hdl;
       help_btn, help_hdl;
       sol_btn, sol_hdl]

  let plot st =
    set_color black;
    let w, _ = text_size !text in
    moveto ((!window_width - w) / 2) ((board_height_i ()) + gap);
    draw_string !text;

    show_buttons st interface_button;
    plot_bsp !bsp

  let color_lines () =
    bsp := B.init (board_width ()) (board_height ()) !bsp;
    adjacency := B.get_lines_area (board_width ()) (board_height ()) !bsp nb_lines;
    let colors = B.colors (board_width ()) (board_height ()) !bsp in
    let change_line_color line left right =
      let size, r, b, g = count line colors in
      let label =
        if Random.int 100 <= S.black_probability
        then {line with line_color = black}
        else if 2 * r > size
        then {line with line_color = red}
        else if 2 * b > size
        then {line with line_color = blue}
        else if 2 * g > size
        then {line with line_color = green}
        else
          let c = (if 4 * r >= size then red else 0) +
                    (if 4 * g >= size then green else 0) +
                    (if 4 * b >= size then blue else 0)
          in {line with line_color = c}
      in
      B.node label left right
    in
    bsp :=
      B.fold (board_width ()) (board_height ())
        change_line_color
        B.region
        !bsp;
    bsp := B.clean (board_width ()) (board_height ()) !bsp

  let view () =
    color_lines ();
    let hdl e =
      if not e.button
      then None
      else if (float_of_int e.mouse_x) < (board_width ()) &&
                (float_of_int e.mouse_y) < (board_height ())
      then begin
          history := ({
                         x = float_of_int e.mouse_x;
                         y = float_of_int e.mouse_y}, 1) :: !history;
          bsp := B.change_color !bsp {
                     x = float_of_int e.mouse_x;
                     y = float_of_int e.mouse_y};
          text := "";
          if is_win ()
          then Some win_page
          else None
        end
      else match List.find_opt (fun (btn, _) -> is_click btn e) interface_button with
           | Some (_, h) ->
              text := "";
              h ()
           | _ -> None
    in hdl

end

let make_game_view (module S : Settings.Game_settings) =
  clear_graph ();
  let module C = Settings.Make_Colors(S) in
  match S.mode with
  | Settings.Classic ->
     let module M = Make(S)(C)(Classic.Bsp(S)(C)) in
     {
       plot = M.plot;
       handler = M.view ()
     }
  | Settings.Extrem ->
     let module M = Make(S)(C)(Extrem.Bsp(S)(C)) in
     {
       plot = M.plot;
       handler = M.view ()
     }
