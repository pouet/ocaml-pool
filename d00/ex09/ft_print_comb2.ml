let ft_print_comb2 () =
    let print_int2 n =
        print_int (n / 10);
        print_int (n mod 10)
    in
    let rec comb_j i j =
        let j = j + 1 in
        if j <= 99 then begin
            print_int2 i;
            print_char ' ';
            print_int2 j;
            if not (i == 98 && j == 99) then begin
                print_char ',';
                print_char ' '
            end;
            comb_j i j
        end
    in
    let rec comb_i i =
        if i <= 99 then begin
            comb_j i i;
            comb_i (i + 1)
        end
    in
    comb_i 0;
    print_char '\n'

let () =
    ft_print_comb2 ()
