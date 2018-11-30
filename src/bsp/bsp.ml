open Graphics
open Geometry

module type Bsp_type = sig

  type bsp

  val change_color : ?reverse:bool -> bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

  val region : color -> bsp

  val node : line -> bsp -> bsp -> bsp
    
  val fold : float -> float -> (line -> 'a -> 'a -> 'a) -> (color -> point list -> 'a) -> bsp -> 'a
    
end

module type Bsp_complete = sig

  include Bsp_type

  val iter_area : (Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

  val iter_line : (line -> unit) -> bsp -> float -> float -> unit

  val clean : float -> float -> bsp -> bsp

end
                     
module Make (B : Bsp_type) = struct

  include B
  
  let iter_area f bsp bound_x bound_y =
    B.fold bound_x bound_y (fun _ _ _ -> ()) f bsp

  let iter_line f bsp bound_x bound_y =
    B.fold bound_x bound_y (fun l _ _ -> f l) (fun _ _ -> ()) bsp

  let clean bound_x bound_y bsp =
    B.fold bound_x bound_y
      B.node
      (fun c _ -> B.region white)
      bsp
                                           
  let generate_random_bsp bound_x bound_y = B.generate_random_bsp bound_x bound_y
    
end
                     
let _ = Random.self_init ()

let colors = [white; red; blue]
let next_color reverse c =
  let rec aux tab = 
    match tab with
    | hd1 :: hd2 :: tl when hd1 = c -> hd2
    | hd :: [] when hd = c -> List.hd colors
    | hd :: tl -> aux tl
    | [] -> aux colors
  in
  let rec aux_r tab = 
    match tab with
    | hd1 :: hd2 :: tl when hd2 = c -> hd1
    | hd :: tl -> aux_r tl
    | [] -> List.nth colors ((List.length colors) - 1)
  in (if reverse then aux_r else aux) colors
  
