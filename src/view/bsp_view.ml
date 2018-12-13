open Geometry
open Graphics
open Bsp
open Interface

let gap = 5
let white_line_width = 5
let colored_line_width = 3

let show_win_page st =
  clear_graph ();
  set_color black;
  moveto 100 100;
  draw_string "winner"

let win_page () =
  show_win_page ();
  {
    plot = show_win_page;
    handler = fun st -> if st.button || st.keypressed then raise Exit; None
  }

type game_mode = Classic | Extrem
type color_mode = RBColor | RGBColor

module type Game_settings = sig
  val mode : game_mode
  val color : color_mode
  val min_area : int
end

module type Bsp_view = sig

  val plot : status -> unit

  val view : unit -> status -> page option

end

module Make (S : Game_settings) (B : Bsp_complete) : Bsp_view = struct

  let bsp, nb_lines =
    let b, nb = B.generate_random_bsp board_width board_height S.min_area in
    ref b, nb

  let adjacency = ref [| |]
  let history = ref []

  let plot_bsp bsp =
    B.iter_area
      (fun label pts ->
        set_color label.color;
        let poly =
          Array.map
            (fun pt -> int_of_float pt.x, int_of_float pt.y)
            (Array.of_list pts)
        in
        fill_poly poly)
      bsp board_width board_height;
    B.iter_line
      (fun l -> draw_line white white_line_width l.section;
             draw_line l.color colored_line_width l.section)
      bsp board_width board_height;
    let _, e = edges board_width board_height in
    List.iter
      (fun x ->
        draw_line white white_line_width x;
        draw_line black colored_line_width x) e

  let count (line: line_label) colors =
    List.fold_left
      (fun (r, b) id ->
        if colors.(id) = red
        then r + 1, b
        else if colors.(id) = blue
        then r, b + 1
        else r, b)
      (0, 0)
      !adjacency.(line.id)

  let is_win () =
    let colors = B.colors board_width board_height !bsp in
    B.fold board_width board_height
      (fun line left right ->
        let r, b = count line colors in
        left && right &&
          ((r > b && line.color = red) ||
             (r < b && line.color = blue) ||
               (r = b && line.color = green)))
      (fun _ -> true)
      !bsp

  let w = 100
  let h = 50
  let y = 625.
  let x_margin = 10.
  let x_pos i = x_margin +. ((2. *. x_margin) +. (float_of_int w)) *. (float_of_int i)

  let quit_btn i = create_button "Quit" (x_pos i) y w h ~bkg:blue ~bkg_hover:red
  let clean_btn i = create_button "Clean" (x_pos i) y w h ~bkg:blue ~bkg_hover:red
  let cancel_btn i = create_button "Cancel" (x_pos i) y w h ~bkg:blue ~bkg_hover:red
  let help_btn i = create_button "Help" (x_pos i) y w h ~bkg:blue ~bkg_hover:red
  let sol_btn i = create_button "Exists solution ?" (x_pos i) y w h ~bkg:blue ~bkg_hover:red

  let no_history_msg = "No history"
  let w_no_hist, h_no_hist = text_size no_history_msg

  let no_solution_msg = "No solution"
  let w_no_sol, h_no_sol = text_size no_solution_msg

  let has_solution_msg = "There is a solution"
  let w_has_sol, h_has_sol = text_size has_solution_msg

  let clean_text () =
    set_color white;
    fill_rect 0 window_width window_width 20

  let help_hdl () =
    let x = B.get_clue board_width board_height !adjacency !bsp in
    let _ = match x with
      | None -> ()
      | Some (n, c) -> begin
          match Bsp.index c, B.find_center !bsp board_width board_height n with
          | _, None -> failwith "Help : incorrect zone number"
          | None, _ -> failwith "Help : incorrect color"
          | Some i, _ when i < 1 -> failwith "Help : incorrect color"
          | Some i, Some pt ->
             history := (pt, i) :: !history;
             bsp := B.color_nth board_width board_height !bsp n c
        end
    in
    if is_win ()
    then Some (win_page ())
    else begin
        plot_bsp !bsp;
        None
      end

  let sol_hdl () =
    set_color black;
    if B.has_solution board_width board_height !adjacency !bsp
    then begin
        moveto ((window_width - w_has_sol) / 2) (board_height_i + gap);
        draw_string has_solution_msg;
      end
    else begin
        moveto ((window_width - w_no_sol) / 2) (board_height_i + gap);
        draw_string no_solution_msg;
      end;
    plot_bsp !bsp;
    None

  let quit_hdl () = raise Exit

  let clean_hdl () =
    history := [];
    bsp := B.clean board_width board_height !bsp;
    plot_bsp !bsp;
    None

  let cancel_hdl () =
    let _ = match !history with
      | [] ->
         set_color black;
         moveto ((window_width - w_no_hist) / 2) (board_height_i + gap);
         draw_string no_history_msg;
      | (pt, n) :: tl ->
         for i = 1 to n do
           bsp := B.change_color ~reverse:true !bsp pt
         done;
         history := tl;
    in
    plot_bsp !bsp;
    None

  let interface_button =
    List.mapi (fun i (btn, h) -> (btn i, h))
      [quit_btn, quit_hdl;
       clean_btn, clean_hdl;
       cancel_btn, cancel_hdl;
       help_btn, help_hdl;
       sol_btn, sol_hdl]

  let plot st =
    show_buttons st interface_button;
    plot_bsp !bsp

  let color_lines () =
    bsp := B.init board_width board_height !bsp;
    adjacency := B.get_lines_area board_width board_height !bsp nb_lines;
    let colors = B.colors board_width board_height !bsp in
    let change_line_color (line: line_label) left right =
      let r, b = count line colors in
      let label : line_label =
        if r > b
        then {line with color = red}
        else if r < b
        then {line with color = blue}
        else {line with color = green}
      in
      B.node label left right
    in
    bsp :=
      B.fold board_width board_height
        change_line_color
        B.region
        !bsp;
    bsp := B.clean board_width board_height !bsp

  let view () =
    color_lines ();
    let hdl e =
      if not e.button
      then None
      else if (float_of_int e.mouse_x) < board_width &&
                (float_of_int e.mouse_y) < board_height
      then begin
          history := ({
                         x = float_of_int e.mouse_x;
                         y = float_of_int e.mouse_y}, 1) :: !history;
          bsp := B.change_color !bsp {
                     x = float_of_int e.mouse_x;
                     y = float_of_int e.mouse_y};
          plot e;
          if is_win ()
          then Some (win_page ())
          else None
        end
      else match List.find_opt (fun (btn, h) -> is_click btn e) interface_button with
           | Some (btn, h) ->
              clean_text ();
              h ()
           | _ -> None
    in hdl

end

let make_game_view (module S : Game_settings) =
  match S.mode with
  | Classic ->
     let module M = Make(S)(Classic.Bsp) in
     {
       plot = M.plot;
       handler = M.view ()
     }
  | Extrem ->
     let module M = Make(S)(Extrem.Bsp) in
     {
       plot = M.plot;
       handler = M.view ()
     }
