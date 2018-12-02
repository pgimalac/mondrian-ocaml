open Geometry

module type Bsp_type = sig

  type bsp

  val change_color : bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

  val iter_area : (Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

  val iter_line : (int -> line -> Graphics.color -> unit) -> bsp -> float -> float -> unit

  val get_lines_area : bsp -> int -> (Graphics.color * ((Graphics.color * int) list)) array

  val get_number_lines : bsp -> int

  val get_number_areas : bsp -> int

end

module Bsp_extrem : Bsp_type
module Bsp_classic : Bsp_type
