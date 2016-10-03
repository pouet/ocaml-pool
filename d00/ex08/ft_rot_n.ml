let is_upper c =
    c >= 'A' && c <= 'Z'

let is_lower c =
    c >= 'a' && c <= 'z'

let rot_lower c n =
    let c = ((int_of_char c - int_of_char 'a') + n) mod 26 in
    char_of_int (c + int_of_char 'a')

let rot_upper c n =
    let c = ((int_of_char c - int_of_char 'A') + n) mod 26 in
    char_of_int (c + int_of_char 'A')

let ft_rot_n n s =
    let rot c =
        if is_upper c then
            rot_upper c n
        else if is_lower c then
            rot_lower c n
        else
            c
    in
    String.map rot s

let () =
    print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 42 "0123456789");
    print_endline (ft_rot_n 2 "OI2EAS67B9");
    print_endline (ft_rot_n 0 "Damned !");
    print_endline (ft_rot_n 42 "");
    print_endline (ft_rot_n 1 "NBzlk qnbjr !");
