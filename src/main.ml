open Bsp
open View

let main () =
  let bsp = ref (generate_random_bsp f_window_width f_window_height 100 (-1)) in
  View.do_with_window
    ~on_open:(fun () -> View.plot_bsp !bsp)
    (fun e ->
      if e.button && window_width > e.mouse_x && e.mouse_y < window_height
      then bsp := change_color !bsp {
               x=float_of_int e.mouse_x;
               y=float_of_int e.mouse_y};

      View.plot_bsp !bsp)

let _ = main ()
