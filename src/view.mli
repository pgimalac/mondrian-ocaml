val do_with_window:
  ?title:string ->
  ?width:int ->
  ?height:int ->
  (unit -> unit) ->
  unit

val plot_on_click: Graphics.status -> unit

val plot_bsp: Bsp.bsp -> unit
    
