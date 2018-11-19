type point = { x : float; y : float; }
type line = { pt1 : point; pt2 : point; }
type bsp = R of Graphics.color | L of line * bsp * bsp

val min_area : float

val coefs : line -> (float * float) option

val is_left : point -> line -> bool

val is_right : point -> line -> bool

val iter : (Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

val insert : float -> float -> bsp -> line -> bsp

val intersect : line -> line -> point option

val change_color : bsp -> point -> bsp

val center : point list -> point

val compare_counter_clockwise : point -> point -> point -> int

val edges : float -> float -> (point list) * (line list)

val generate_random_bsp : float -> float -> int -> int -> bsp

val generate_random_bsp_maxime_dont_work : float -> float -> bsp

val print_point : point -> unit

val print_bsp : bsp -> unit

val print_line : line -> unit

val find_angle : point -> float

val dist : point -> point -> float

val separate_points : line -> (point list * point list) -> point list -> (point list * point list)
