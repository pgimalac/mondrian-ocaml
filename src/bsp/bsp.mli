(**
 * File containting generic Bsp interface
 *)

(**
 * Module to implement a bsp
 *)
module type Bsp_type = sig
  type bsp
  (** abstract over the bsp's implementation *)

  val change_color : ?reverse:bool -> bsp -> Geometry.point -> bsp
  (**
   * change the color of the area containing the given point
   * if reverse is specified to true, the color is change following
   * the reversed order, used to cancel an action *)

  val generate_random_bsp : float -> float -> int -> int -> bsp * int
  (** Generate a complete random bsp *)

  val region : Geometry.region_label -> bsp
  (** Constructor for a leaf of the bsp *)

  val node : Geometry.line_label -> bsp -> bsp -> bsp
  (** Constructor for a node of the bsp *)

  (** theses two functions are used to define generic
      function un Bsp_complete *)

  val fold :
    float ->
    float ->
    (Geometry.line_label -> 'a -> 'a -> 'a) ->
    (Geometry.region_label -> 'a) ->
    bsp ->
    'a
  (** apply a function on a bsp
      the first function create a value from the value returned
      by the left and right node, the second function return a value
      for a given area (based on it color and polygon) *)

  val iter :
    float ->
    float ->
    (Geometry.line_label -> 'a -> 'a * 'a) ->
    (Geometry.region_label -> 'a -> unit) ->
    'a ->
    bsp ->
    unit
  (** apply a side effect on a region
      based on accumulators generated with adjacents lines *)
end

(**
 * More generic module
 * aim to facotize code
 *)
module type Bsp_complete = sig
  include Bsp_type

  val iter_area :
    (Geometry.region_label -> Geometry.point list -> unit) ->
    bsp ->
    float ->
    float ->
    unit
  (** apply a function on each areas *)

  val iter_line : (Geometry.line_label -> unit) -> bsp -> float -> float -> unit
  (** apply a function on each line*)

  val clean : float -> float -> bsp -> bsp
  (** clear all color (all areas are set to white) *)

  val get_lines_area : float -> float -> bsp -> int -> int list array

  val for_all_lines :
    float -> float -> (Geometry.line_label -> bool) -> bsp -> bool

  val init : float -> float -> bsp -> bsp
  (** set unique id for each region and line *)

  (** To use following function, the bsp must be initialized *)

  val colors : float -> float -> bsp -> int array
  (** return an array containing at index i the color of the associated region *)

  val color_nth : float -> float -> bsp -> int -> Graphics.color -> bsp
  (** change the colors of the region with the given ID *)

  val get_fnc :
    float ->
    float ->
    int list array ->
    bsp ->
    (bool * (int * Graphics.color)) list list
  (** generate an conjonctive normal form representing
      the problem of his coloration
      Should this function be in the interface ?
      TODO: add some details here  *)

  val get_solution :
    float ->
    float ->
    int list array ->
    bsp ->
    (bool * (int * Graphics.color)) list option
  (** for a given bsp return a coloration if it exists
      return None otherwise *)

  val get_clue :
    float -> float -> int list array -> bsp -> (int * Graphics.color) option
  (** for a given bsp return a region id and a color leading to a solution
      if no solution exists return None *)

  val has_solution : float -> float -> int list array -> bsp -> bool
  (** return true iff the given bsp can be completed in a valid coloration *)

  val is_solution : float -> float -> int list array -> bsp -> bool
  (** return true iff the given bsp is fully and correctly colored *)

  val find_center : bsp -> float -> float -> int -> Geometry.point option
  (** gives the center of the area whose number is given *)
end

(** Construct a complete bsp from the minimum implementation *)
module Make : functor (S : Settings.Game_settings) (B : Bsp_type) ->
  Bsp_complete
