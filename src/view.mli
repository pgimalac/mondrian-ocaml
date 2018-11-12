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

val plot_bsp: Bsp.bsp -> unit
    
