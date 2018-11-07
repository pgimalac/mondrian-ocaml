type color = Blue | Red
type point = { x : float; y : float; }
type line = { pt : point; theta: float; c : color; }
type bsp = R of color option | L of line * bsp * bsp

val is_above: point -> line -> bool
val is_below: point -> line -> bool
val is_left: point -> line -> bool
val is_right: point -> line -> bool
