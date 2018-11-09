val do_with_window:
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?on_open:(unit -> unit) ->
  (Graphics.status -> unit) ->
  unit

val plot_bsp: Bsp.bsp -> unit
    
