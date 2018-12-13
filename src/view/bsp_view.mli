(** Differents types of gameplay *)

(** This type denote the kind of bsp you want to play with *)
type game_mode = Classic | Extrem

(** This type denote the nymber of colors you want in your bsp's regions
    RBColor is 2 colors: red and blue
    RGBColor is the extented version with 3 colors: red blue and green*)
type color_mode = RBColor | RGBColor

(** This module represents the configuration of the game for a single game *)
module type Game_settings = sig

  (** type of bsp *)
  val mode : game_mode

  (** number of colors *)
  val color : color_mode

  (** mininum area of a region *)
  val min_area : int
end

(** Return game page for given game settings*)
val make_game_view : (module Game_settings) -> Interface.page
