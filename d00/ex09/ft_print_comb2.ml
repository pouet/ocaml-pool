let print_int2 n =
    print_int (n / 10);
    print_int (n mod 10)

let ft_print_comb2 () =
    for i = 0 to 99 do
        for j = i + 1 to 99 do
            print_int2 i;
            print_char ' ';
            print_int2 j;
            if not (i == 98 && j == 99) then begin
                print_char ',';
                print_char ' '
            end
        done;
    done;
    print_char '\n'

let () =
    ft_print_comb2 ()
