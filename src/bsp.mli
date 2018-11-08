type color = Blue | Red
type point = { x : float; y : float; }
type line = { pt1 : point; pt2 : point; c : color; }
type bsp = R of color option | L of line * bsp * bsp

val insert : bsp -> line -> bsp
