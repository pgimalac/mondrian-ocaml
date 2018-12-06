open Geometry

module type Bsp_type = sig

  type bsp

  val change_color : ?reverse:bool -> bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

  val color_nth : bsp -> int -> Graphics.color -> bsp

  val iter_area : (int -> Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

  val iter_line : (int -> line -> Graphics.color -> unit) -> bsp -> float -> float -> unit

  val iter_line_area : (int -> line -> Graphics.color -> unit) -> (int -> Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

  val get_lines_area : bsp -> int -> (Graphics.color * ((Graphics.color * int) list)) array

  val get_number_lines : bsp -> int

  val get_number_areas : bsp -> int

  val clean : bsp -> bsp

  val get_fnc : bsp -> (bool * int) list list

  val get_solution : bsp -> (bool * int) list option

  val get_clue : bsp -> bsp

  val has_solution : bsp -> bool

  val is_solution : bsp -> bool

end

module Bsp_extrem : Bsp_type
module Bsp_classic : Bsp_type
