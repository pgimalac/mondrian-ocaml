val window_width : int
val window_height : int

val f_window_width : float
val f_window_height : float

val do_with_window:
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?on_open:(unit -> unit) ->
  (Graphics.status -> unit) ->
  unit

type game_mode = Classic | Extrem
  
val menu : Graphics.status option -> game_mode option

module type Bsp_view = sig

  val plot : unit -> unit
  
  val view : unit -> Graphics.status -> unit

end

module Make : functor (B : Bsp.Bsp_type) -> Bsp_view
