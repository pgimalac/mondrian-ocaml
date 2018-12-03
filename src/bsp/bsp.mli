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
             (Geometry.region_label -> Geometry.point list -> 'a) ->
             bsp -> 'a

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

  val init : float -> float -> bsp -> bsp

  val colors : float -> float -> bsp -> int array
end

(** Construct a complete bsp from the minimum implementation *)
module Make : functor (B : Bsp_type) -> Bsp_complete

(** iter through colors, in reversed order if the given boolean is true *)
val next_color : bool -> Graphics.color -> Graphics.color

(** return a random color among non-white colors *)
val rand_color : unit -> Graphics.color
