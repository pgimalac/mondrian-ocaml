open Bsp
open View
open Graphics
   
let main () =
  let bsp = ref (generate_random_bsp f_window_width f_window_height 5) in
  View.do_with_window
    ~on_open:(fun () -> View.plot_bsp !bsp)
    (fun e ->
      if e.button
      then bsp := change_color !bsp {
               x=float_of_int e.mouse_x;
               y=float_of_int e.mouse_y};
      
      View.plot_bsp !bsp)

let _ = main ()
