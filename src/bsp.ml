type color = Blue | Red
type point = { x : float; y : float; }
type line = { pt : point; theta : float; c : color; }
type bsp = R of color option | L of line * bsp * bsp

let is_above pt line = true

let is_below pt line = not (is_above pt line)

let is_left pt line = true

let is_right pt line = not (is_left pt line)
