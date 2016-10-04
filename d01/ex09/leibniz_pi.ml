let leibniz_pi delta =
    let pi = 4. *. (atan 1.) in
    let abs n =
        if n < 0. then (-1. *. n)
        else n
    in
    let getval i =
        let i = float_of_int i in
        4. *. (-1. ** i) /. (2. *. i +. 1.)
    in
    let rec aux i acc =
        let tmp = getval (i + 1) in
        if abs (acc -. pi) < delta then i
        else aux (i + 1) (acc +. tmp)
    in
    if delta <= 0. then (-1)
    else aux 0 (getval 0)

let test n =
    print_int (leibniz_pi n);
    print_endline ""

let () =
    test (-1.);
    test 1.;
    test 2.;
    test 1e-6
