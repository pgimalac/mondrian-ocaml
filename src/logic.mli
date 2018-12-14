val set_three_colors : bool -> unit
val get_three_colors : unit -> bool

val antilogy : (bool * (int * Graphics.color)) list list
val tautology : (bool * (int * Graphics.color)) list list

val get_function_color : Graphics.color -> (int -> int -> int -> int -> int list -> (bool * (int * Graphics.color)) list list)

val basics : unit -> (int -> (bool * (int * Graphics.color)) list list)
