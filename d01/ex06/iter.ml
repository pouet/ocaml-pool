let rec iter f x n =
    if n < 0 then -1
    else if n == 0 then x
    else f (iter f x (n - 1))

let test f n m =
    let tmp = iter f n m in
    print_int tmp;
    print_endline ""

let () =
    test (fun x -> x * x) 2 4;
    test (fun x -> x * x) 5 (-1);
    test (fun x -> x * 2) 2 4;
    test (fun x -> x * 2) 4 0
