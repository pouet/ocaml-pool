let ft_string_all f s =
    let len = String.length s in
    let rec aux = function
        | 0 -> true
        | i ->
                if f s.[i] == false then false
                else aux (i - 1)
    in
    if len == 0 then
        false
    else
        aux (len - 1)


let is_digit c =
    c >= '0' && c <= '9'

let print_bool = function
    | true -> print_endline "true"
    | false -> print_endline "false"

let () =
    print_bool (ft_string_all is_digit "0123456789");
    print_bool (ft_string_all is_digit "0123y56789");
    print_bool (ft_string_all is_digit "");
