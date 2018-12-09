open Graphics
open Geometry
open Bsp

let gap = 5
let white_line_width = 5
let colored_line_width = 3

let window_width = 600
let window_height = 700

let board_width_i = window_width
let board_height_i = window_width

let board_width = float_of_int board_width_i
let board_height = float_of_int board_height_i

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
  moveto ((int_of_float btn.pt.x) + (btn.w - w) / 2)
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

type game_mode = Classic | Extrem

let menu st =
  let title = "Mondrian" in
  let w, h = text_size title in
  let x_title = (window_width - w) / 2 in
  let y_title = 450 in

  let buttons =
    [
      create_button "Classic mode" 150. 325. 250 75, (fun st -> Some Classic);
      create_button "Extrem mode" 150. 200. 250 75, (fun st -> Some Extrem);
      create_button "Quit" 150. 75. 250 75, fun st -> raise Exit
    ] in

  let print_menu hovers =
    set_color black;
    moveto x_title y_title;
    draw_string title;
    List.iter2 (fun (btn, _) is_hover-> print_btn ~hover:is_hover btn) buttons hovers
  in
  match st with
  | Some st when st.button ->
     begin
       match List.find_opt (fun (btn, _) -> is_click btn st) buttons with
       | Some (btn, hdl) -> hdl st
       | None ->
          print_menu (List.map (fun (btn, _) -> is_click btn st) buttons);
          None
     end
  | Some st ->
     print_menu (List.map (fun (btn, _) -> is_click btn st) buttons);
     None
  | _ ->
     print_menu (List.map (fun _ -> false) buttons);
     None

module type Bsp_view = sig

  val plot : unit -> unit

  val view : unit -> Graphics.status -> unit

end

module Make (B : Bsp_complete) : Bsp_view = struct

  let bsp, nb_lines =
    let b, nb = B.generate_random_bsp board_width board_height in
    ref b, nb

  let adjacency = ref [| |]
  let history = ref []

  let clean_text () =
    set_color white;
    fill_rect 0 window_width window_width 100

  let interface_button =
    let w = 100 in
    let h = 50 in
    let y = 625. in
    let x_margin = 10. in
    let x_pos i = x_margin +. ((2. *. x_margin) +. (float_of_int w)) *. (float_of_int i) in

    let quit_btn i = create_button "Quit" (x_pos i) y w h ~bkg:blue in
    let clean_btn i = create_button "Clean" (x_pos i) y w h ~bkg:blue in
    let cancel_btn i = create_button "Cancel" (x_pos i) y w h ~bkg:blue in
    let help_btn i = create_button "Help" (x_pos i) y w h ~bkg:blue in
    let sol_btn i = create_button "Exists solution ?" (x_pos i) y w h ~bkg:blue in

    let no_history_msg = "No history" in
    let w_no_hist, h_no_hist = text_size no_history_msg in

    let no_solution_msg = "No solution" in
    let w_no_sol, h_no_sol = text_size no_solution_msg in

    let has_solution_msg = "There is a solution" in
    let w_has_sol, h_has_sol = text_size has_solution_msg in

    let help_hdl () =
      let x = B.get_clue board_width board_height !adjacency !bsp in
      match x with
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
    let sol_hdl () =
      clean_text ();
      set_color black;
      if B.has_solution board_width board_height !adjacency !bsp
      then begin
        moveto ((window_width - w_has_sol) / 2) (board_height_i + gap);
        draw_string has_solution_msg
      end
      else begin
        moveto ((window_width - w_no_sol) / 2) (board_height_i + gap);
        draw_string no_solution_msg
      end
    in
    let quit_hdl () = raise Exit in
    let clean_hdl () =
      history := [];
      bsp := B.clean board_width board_height !bsp in
    let cancel_hdl () =
      match !history with
      | [] ->
         clean_text ();
         set_color black;
         moveto ((window_width - w_no_hist) / 2) (board_height_i + gap);
         draw_string no_history_msg
      | (pt, n) :: tl ->
        for i = 1 to n do
          bsp := B.change_color ~reverse:true !bsp pt
        done;
        history := tl;
    in
    List.mapi (fun i (btn, h) -> (btn i, h))
      [quit_btn, quit_hdl;
       clean_btn, clean_hdl;
       cancel_btn, cancel_hdl;
       help_btn, help_hdl;
       sol_btn, sol_hdl]

  let plot_bsp bsp =
    B.iter_area
      (fun label pts ->
        set_color label.color;
        let poly =
          Array.map
            (fun pt -> int_of_float pt.x, int_of_float pt.y) (Array.of_list pts)
        in
        fill_poly poly)
      bsp board_width board_height;
    B.iter_line
      (fun l -> draw_line white white_line_width l.section;
             draw_line l.color colored_line_width l.section)
      bsp board_width board_height;
    let _, e = edges board_width board_height in
    List.iter (fun x -> draw_line white white_line_width x; draw_line black colored_line_width x) e

  let plot () =
    List.iter (fun (btn, _) -> print_btn btn) interface_button;
    plot_bsp !bsp

  let color_lines () =
    bsp := B.init board_width board_height !bsp;
    adjacency := B.get_lines_area board_width board_height !bsp nb_lines;
    let colors = B.colors board_width board_height !bsp in
    bsp :=
      B.fold board_width board_height
        (fun line left right ->
          let r, b =
            List.fold_left
              (fun (r, b) id ->
                if colors.(id) = red
                then r + 1, b
                else if colors.(id) = blue
                then r, b + 1
                else r, b)
              (0, 0)
              !adjacency.(line.id)
          in
          let label =
            if r > b
            then {line with color = red}
            else if r < b
            then {line with color = blue}
            else {line with color = green}
          in
          B.node label left right)
        B.region
        !bsp;
    bsp := B.clean board_width board_height !bsp

  let view () =
    color_lines ();
    let hdl e =
      if e.button
      then begin
          if (float_of_int e.mouse_x) < board_width &&
               (float_of_int e.mouse_y) < board_height
          then begin
              history := ({
                x = float_of_int e.mouse_x;
                y = float_of_int e.mouse_y}, 1) :: !history;
              bsp := B.change_color !bsp {
                         x = float_of_int e.mouse_x;
                         y = float_of_int e.mouse_y};
            end
          else if e.button
          then begin
              match List.find_opt (fun (btn, h) -> is_click btn e) interface_button with
              | Some (btn, h) -> h ()
              | _ -> ()
            end;
          plot ()
        end
    in hdl

end
