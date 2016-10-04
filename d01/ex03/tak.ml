let rec tak x y z =
    if y < x then begin
        let tmp1 = tak (x - 1) y z
        and tmp2 = tak (y - 1) z x
        and tmp3 = tak (z - 1) x y in
        tak tmp1 tmp2 tmp3
    end
    else z

let test x y z =
    print_int (tak x y z);
    print_endline ""

let () =
    test 1 2 3;
    test 5 23 7;
    test 9 1 0;
    test 1 1 1;
    test 0 42 0;
    test 23498 98734 98776
