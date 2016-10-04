let fibonacci n =
    let rec aux m acc1 acc2 =
        if m == n then acc1
        else aux (m + 1) (acc2) (acc1 + acc2)
    in
    if n < 0 then -1
    else aux 0 0 1

let test n =
    let n = fibonacci n in
    print_int n;
    print_endline ""

let () =
    test (-42);
    for i = 0 to 10 do
        test i
    done;
    test 30
