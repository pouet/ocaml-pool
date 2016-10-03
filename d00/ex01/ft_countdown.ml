let ft_countdown n =
    if n < 0 then begin
        print_int 0;
        print_char '\n';
    end;
    for i = n downto 0 do
        print_int i;
        print_char '\n'
    done

let test n =
    print_string "Test with [";
    print_int n;
    print_string "]: ";
    ft_countdown n

let () =
    test (-1);
    test 0;
    test 3;
    test 10
