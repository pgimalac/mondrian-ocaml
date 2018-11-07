val do_with_window:
  ?title:string ->
  ?width:int ->
  ?height:int ->
  (Graphics.status -> unit) ->
  unit

val plot_on_click: Graphics.status -> unit
    
