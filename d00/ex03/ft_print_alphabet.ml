let ft_print_alphabet () =
    let a = int_of_char 'a'
    and z = int_of_char 'z' in
    for i = a to z do
        let c = char_of_int i in
        print_char c
    done;
    print_char '\n'

let _ =
    ft_print_alphabet ()
