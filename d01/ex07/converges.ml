let rec converges f x n =
    if n < 0 then false
    else if n == 0 then (f x) == x
    else converges f (f x) (n - 1)

let test f n m =
    let tmp = converges f n m in
    if tmp then print_endline "true"
    else print_endline "false"

let () =
    test (( * ) 2) 2 5;
    test (fun x -> x / 2) 2 3;
    test (fun x -> x / 2) 2 2
