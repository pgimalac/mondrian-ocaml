open Graphics
open Geometry

module type Bsp_type = sig

  type bsp
  
  val change_color : bool -> bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

  val region : color -> bsp

  val node : line -> bsp -> bsp -> bsp
    
  val fold : float -> float -> (line -> 'a -> 'a -> 'a) -> (color -> point list -> 'a) -> bsp -> 'a

end

module type Bsp_complete = sig

  type bsp
     
  val iter_area : (Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

  val iter_line : (line -> unit) -> bsp -> float -> float -> unit

  val clean : float -> float -> bsp -> bsp
    
  val change_color : ?reverse:bool -> bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

end
                     
module Make : functor (B : Bsp_type) -> Bsp_complete

val next_color : bool -> Graphics.color -> Graphics.color
