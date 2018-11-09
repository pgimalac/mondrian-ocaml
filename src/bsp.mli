type color = Blue | Red
type point = { x : float; y : float; }
type line = { pt1 : point; pt2 : point; }
type bsp = R of color option | L of line * bsp * bsp

val pi : float

val coefs : line -> (float * float) option
  
val is_left : point -> line -> bool

val is_right : point -> line -> bool
                                  
val insert : bsp -> line -> bsp

val intersect : line -> line -> point option
  
val change_color : bsp -> point -> bsp
