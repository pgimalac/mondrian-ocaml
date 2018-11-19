open Geometry

module type Bsp_type = sig

  type bsp
  
  val change_color : bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

  val iter_area : (Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

  val iter_line : (line -> unit) -> bsp -> float -> float -> unit

end

module Bsp_extrem : Bsp_type

module Bsp_classic : Bsp_type
