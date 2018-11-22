type ratio = {
    num : int;
    den : int;
}

val ratio_of_int : int -> ratio

val float_of_ratio : ratio -> float

val random_ratio : float -> ratio

val int_of_ratio : ratio -> int

val ratio_build : int -> int -> ratio

val string_of_ratio : ratio -> string

val ( *@ ) : ratio -> ratio -> ratio

val ( /@ ) : ratio -> ratio -> ratio

val ( +@ ) : ratio -> ratio -> ratio

val ( -@ ) : ratio -> ratio -> ratio

val ( <=@ ) : ratio -> ratio -> bool

val ( >=@ ) : ratio -> ratio -> bool

val ( <@ ) : ratio -> ratio -> bool

val ( <@ ) : ratio -> ratio -> bool

val ratio0 : ratio

val ratio1 : ratio

val compare_ratio : ratio -> ratio -> int
