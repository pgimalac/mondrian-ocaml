open Geometry

module type Bsp_type = sig

  type bsp
  
  val change_color : ?reverse:bool -> bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

  val iter_area : (Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

  val iter_line : (line -> unit) -> bsp -> float -> float -> unit

  val clean : bsp -> bsp

end

val next_color : bool -> Graphics.color -> Graphics.color
