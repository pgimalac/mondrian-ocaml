val antilogy : (bool * (int * Graphics.color)) list list
val tautology : (bool * (int * Graphics.color)) list list

val get_red : int -> int list -> (bool * (int * Graphics.color)) list list
val get_green : int -> int list -> (bool * (int * Graphics.color)) list list
val get_blue : int -> int list -> (bool * (int * Graphics.color)) list list

val get_yellow : int -> int -> int -> int -> int -> int list -> (bool * (int * Graphics.color)) list list
val get_cyan : int -> int -> int -> int -> int -> int list -> (bool * (int * Graphics.color)) list list
val get_magenta : int -> int -> int -> int -> int -> int list -> (bool * (int * Graphics.color)) list list

val get_black : int -> int -> int -> int -> int -> int -> int list -> (bool * (int * Graphics.color)) list list

val number_of_colors : Graphics.color -> int

val basics : int -> (bool * (int * Graphics.color)) list list
