(** Global definitions for all pages *)

val window_width : int ref
(** dimensions of the window *)

val window_height : int ref

val board_width_i : unit -> int
(** dimensions of the bsp area in game *)

val board_height_i : unit -> int

val board_width : unit -> float

val board_height : unit -> float

exception Exit
(** Global exception to exit the game *)

type page = {
  plot : Graphics.status -> unit;
  handler : Graphics.status -> page option;
}
(** generic record for a page
    plot render the page on the screen
    handler receive an event an can change page, otherwise return None *)

type button = {
  text : string;
  pt : Geometry.point;
  w : int;
  h : int;
  bkg : Graphics.color;
  bkg_hover : Graphics.color;
  fg : Graphics.color;
  fg_hover : Graphics.color;
}
(** Describe a button on the screen, his position, text and color(s) *)

val print_btn : ?hover:bool -> button -> unit
(** print the button on the screen *)

val create_button :
  ?bkg:Graphics.color ->
  ?bkg_hover:Graphics.color ->
  ?fg:Graphics.color ->
  ?fg_hover:Graphics.color ->
  string ->
  float ->
  float ->
  int ->
  int ->
  button
(** create a new button *)

val is_click : button -> Graphics.status -> bool
(** return if user's mouse is over the given button *)

val show_buttons : Graphics.status -> (button * 'a) list -> unit
(** render a list of buttons *)

val buttons_groups :
  (button * (unit -> 'a)) list -> (button * (unit -> 'a)) list * (unit -> unit)
(** get a list of buttons and group them on the interface
    return 2 functions, the first one is the new list of button
    and the second is a function to rendrer the group of buttons
    the selected button is rendred as hover*)
