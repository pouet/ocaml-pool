let ft_sum f x y =
    let rec aux i acc =
        if i <= y then aux (i + 1) (acc +. f i)
        else acc
    in
    if x > y then nan
    else aux x 0.

let () =
    print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
    print_endline "";
    print_float (ft_sum (fun i -> float_of_int (i * i)) 10 1);
    print_endline ""
