(**
 * File containting generic Bsp interface
 *)

(**
 * Module to implement a bsp
 *)
module type Bsp_type = sig

  (** abstract over the bsp's implementation *)
  type bsp

  (**
   * change the color of the area containing the given point
   * if reverse is specified to true, the color is change following
   * the reversed order, used to cancel an action *)
  val change_color : ?reverse:bool -> bsp -> Geometry.point -> bsp

  (** Generate a complete random bsp *)
  val generate_random_bsp : float -> float -> bsp * int

  (** Constructor for a leaf of the bsp *)
  val region : Geometry.region_label -> bsp

  (** Constructor for a node of the bsp *)
  val node : Geometry.line_label -> bsp -> bsp -> bsp

  (** theses two functions are used to define generic
      function un Bsp_complete *)

  (** apply a function on a bsp
      the first function create a value from the value returned
      by the left and right node, the second function return a value
      for a given area (based on it color and polygon) *)
  val fold : float -> float ->
             (Geometry.line_label -> 'a -> 'a -> 'a) ->
             (Geometry.region_label -> 'a) ->
             bsp -> 'a

  (** apply a side effect on a region
      based on accumulators generated with adjacents lines *)
  val iter : float -> float ->
             (Geometry.line_label -> 'a -> 'a * 'a) ->
             (Geometry.region_label -> 'a -> unit) ->
             'a -> bsp -> unit

end

(**
 * More generic module
 * aim to facotize code
 *)
module type Bsp_complete = sig

  include Bsp_type

  (** apply a function on each areas *)
  val iter_area : (Geometry.region_label -> Geometry.point list -> unit) ->
                  bsp -> float -> float -> unit

  (** apply a function on each line*)
  val iter_line : (Geometry.line_label -> unit) ->
                  bsp -> float -> float -> unit

  (** clear all color (all areas are set to white) *)
  val clean : float -> float -> bsp -> bsp

  val get_lines_area : float -> float -> bsp -> int -> int list array

  (** set unique id for each region and line *)
  val init : float -> float -> bsp -> bsp

  (** To use following function, the bsp must be initialized *)

  (** return an array containing at index i the color of the associated region *)
  val colors : float -> float -> bsp -> int array

  (** change the colors of the region with the given ID *)
  val color_nth : float -> float ->
                  bsp -> int -> Graphics.color ->
                  bsp

  (** generate an conjonctive normal form representing
      the problem of his coloration
      Should this function be in the interface ?
      TODO: add some details here  *)
  val get_fnc : float -> float ->
                int list array -> bsp ->
                (bool * (int * Graphics.color)) list list

  (** for a given bsp return a coloration if it exists
      return None otherwise *)
  val get_solution : float -> float ->
                     int list array -> bsp ->
                     (bool * (int * Graphics.color)) list option

  (** for a given bsp return a region id and a color leading to a solution
      if no solution exists return None *)
  val get_clue : float -> float ->
                 int list array -> bsp ->
                 (int * Graphics.color) option

  (** return true iff the given bsp can be completed in a valid coloration *)
  val has_solution : float -> float ->
                     int list array -> bsp ->
                     bool

  (** return true iff the given bsp is fully and correctly colored *)
  val is_solution : float -> float ->
                    int list array -> bsp ->
                    bool

  (** gives the center of the area whose number is given *)
  val find_center : bsp -> float -> float -> int -> Geometry.point option

end

(** Construct a complete bsp from the minimum implementation *)
module Make : functor (B : Bsp_type) -> Bsp_complete

(** returns the index of the given color into the list, None if it isn't in *)
val index : Graphics.color -> int option

(** iter through colors, in reversed order if the given boolean is true *)
val next_color : bool -> Graphics.color -> Graphics.color

(** return a random color among non-white colors *)
val rand_color : unit -> Graphics.color
