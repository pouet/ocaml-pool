let ft_print_comb () =
    let rec for_k i j k =
        print_int i;
        print_int j;
        print_int k;
        if not (i == 7 && j == 8 && k == 9) then
            print_string ", ";
        if k < 9 then for_k i j (k + 1)
    in 
    let rec for_j i j = 
        if j < 9 then begin
            for_k i j (j + 1);
            for_j i (j + 1)
        end
    in
    let rec for_i i =
        if i < 9 then begin
            for_j i (i + 1);
            for_i (i + 1)
        end
    in
    for_i 0;
    print_string "\n"

let () =
    ft_print_comb ()
