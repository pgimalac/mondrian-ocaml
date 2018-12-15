(**
 * View contains all graphics function
 *)

(** Open a window and execute the given
    function each time an event is fired *)
val do_with_window:
  ?title:string ->
  ?on_open:(unit -> unit) ->
  (Graphics.status -> unit) ->
  unit

(** Differents types of gameplay
    coincide with differents implentations of the
    bsp modules *)
type game_mode = Classic | Extrem

(** Menu page *)
val menu : Graphics.status option -> game_mode option

(** Module to plot on the window a given game mode *)
module type Bsp_view = sig

  (** plot the user interface *)
  val plot : unit -> unit

  (** generate a bsp and return the handler to interact with the events *)
  val view : unit -> Graphics.status -> unit

end

(** Functor to create user interface functions from bsp generic implementation *)
module Make : functor (B : Bsp.Bsp_complete) -> Bsp_view

val set_window_width : int -> unit
val set_window_height : int -> unit
