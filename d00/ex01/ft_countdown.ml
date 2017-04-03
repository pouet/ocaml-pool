let rec ft_countdown n =
    if n <= 0 then begin
        print_int 0;
        print_char '\n';
    end
    else begin
        print_int n;
        print_char '\n';
        ft_countdown (n - 1)
    end

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
