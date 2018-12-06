(* we consider red = true and blue = false *)


let antilogy = [[(true, 0)]; [(false, 0)]]

let tautology = []

(* all subsets of size k of l *)
let select b k l =
    let e = ref [] in
    List.iter (fun (n, l) -> if n = k then e := l :: !e)
    (
        List.fold_left (fun acc h ->
            List.fold_left (fun acc (n, l) ->
                if n = k
                then begin
                    e := l :: !e;
                    acc
                end
                else (n + 1, (b, h) :: l) :: (n, l) :: acc
            ) [] acc
        ) [(0, [])] l;
    ); !e

(* at least k variables of l are b *)
let at_least b k l =
    select b (List.length l + 1 - k) l

(* not tail-recursive but heap space linear to the size of l *)
(* so less than the number of areas -- so ok *)
let split b n1 l =
    let size = List.length l in
    let n2 = size - n1 in
    if n1 < 0 || n2 < 0
    then begin
            print_endline "split number error";
            antilogy
         end
    else
        let x1 = at_least b n1 l in
        let x2 = at_least (not b) n2 l in
        List.rev_append x1 x2
