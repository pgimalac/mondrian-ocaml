type ratio = {
    num : int;
    den : int;
}

let pgcd a b =
    let aa = if a < 0 then -a else a in
    let bb = if b < 0 then -b else b in
    let rec gcd a b =
        if a = 0
        then b
        else gcd (b mod a) a
    in
        if aa < bb
        then gcd aa bb
        else gcd bb aa

let ratio_of_int i =
    {num = i; den = 1}

let float_of_ratio r =
    (float_of_int r.num) /. (float_of_int r.den)

let int_of_ratio r =
    r.num / r.den

let ratio_build x y =
    if y = 0 then failwith "Division by zero."
    else
        let s = if y < 0 then -1 else 1 in
        let d = pgcd x y in
        let xx = s * x in
        let yy = s * y in
        {num = xx / d; den = yy / d}

let random_ratio f =
    let p = Random.float f in
    ratio_build (int_of_float (p *. 1000.)) 1000

let string_of_ratio r =
    string_of_int r.num ^ "/" ^ (string_of_int r.den)

let compare_ratio r s =
    r.num * s.den - s.num * r.den

let ( *@ ) r s =
    ratio_build (r.num * s.num) (r.den * s.den)

let ( /@ ) r s =
    ratio_build (r.num * s.den) (r.den * s.num)

let ( +@ ) r s =
    ratio_build (r.num * s.den + s.num * r.den) (r.den * s.den)

let ( -@ ) r s =
    ratio_build (r.num * s.den - s.num * r.den) (r.den * s.den)

let ( <=@ ) r s =
    r.num * s.den <= r.den * s.num

let ( >=@ ) r s =
    r.num * s.den >= r.den * s.num

let ( <@ ) r s =
    r.num * s.den < r.den * s.num

let ( >@ ) r s =
    r.num * s.den > r.den * s.num

let ratio0 = ratio_of_int 0
let ratio1 = ratio_of_int 1
