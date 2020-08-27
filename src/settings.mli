(** Differents types of gameplay *)

(** This type denote the kind of bsp you want to play with *)
type game_mode = Classic | Extrem

(** This type denote the nymber of colors you want in your bsp's regions
    RGColor is 2 colors: red and blue
    RGBColor is the extented version with 3 colors: red blue and green*)
type color_mode = RGColor | RGBColor

(** This module represents the configuration of the game for a single game *)
module type Game_settings = sig
  val mode : game_mode
  (** type of bsp *)

  val color : color_mode
  (** number of colors *)

  val min_area : int
  (** mininum area of a region *)

  val black_probability : int
end

module type Colors = sig
  val index : Graphics.color -> int option
  (** returns the index of the given color into the list, None if it isn't in *)

  val next_color : bool -> Graphics.color -> Graphics.color
  (** iter through colors, in reversed order if the given boolean is true *)

  val rand_color : unit -> Graphics.color
  (** return a random color among non-white colors *)
end

module Make_Colors : functor (S : Game_settings) -> Colors
