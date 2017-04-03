let ft_print_alphabet () =
    let a = int_of_char 'a'
    and z = int_of_char 'z' in
    let rec print c =
        if c <= z then begin
            print_char (char_of_int c);
            print (c + 1)
        end
        else print_char '\n'
    in
    print a

let _ =
    ft_print_alphabet ()
