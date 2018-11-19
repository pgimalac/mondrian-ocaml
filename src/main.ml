open Geometry
open Bsp
open View
open Graphics

let plot bsp =
  Bsp_classic.iter_area
    (fun color pts ->
      set_color color;
      let poly =
        Array.map
          (fun pt -> int_of_float pt.x, int_of_float pt.y) (Array.of_list pts)
      in Graphics.fill_poly poly)
    bsp f_window_width f_window_height;
  Bsp_classic.iter_line (draw_line white 3) bsp f_window_width f_window_height;
  Bsp_classic.iter_line (draw_line black 2) bsp f_window_width f_window_height

let plot_extrem bsp =
  Bsp_extrem.iter_area
    (fun color pts ->
      set_color color;
      let poly =
        Array.map
          (fun pt -> int_of_float pt.x, int_of_float pt.y) (Array.of_list pts)
      in Graphics.fill_poly poly)
    bsp f_window_width f_window_height;
  Bsp_extrem.iter_line (draw_line white 3) bsp f_window_width f_window_height;
  Bsp_extrem.iter_line (draw_line black 2) bsp f_window_width f_window_height
   
let main () =
  (* let bsp = ref (Bsp_classic.generate_random_bsp f_window_width f_window_height) in *)
  let bsp = ref (Bsp_extrem.generate_random_bsp f_window_width f_window_height) in 
  View.do_with_window
    ~on_open:(fun () -> plot_extrem !bsp)
    (fun e ->
      if e.button && window_width > e.mouse_x && e.mouse_y < window_height
      then bsp := Bsp_extrem.change_color !bsp {
               x=float_of_int e.mouse_x;
               y=float_of_int e.mouse_y};
      plot_extrem !bsp)

let _ = main ()
