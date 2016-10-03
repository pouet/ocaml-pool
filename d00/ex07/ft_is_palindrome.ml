let ft_is_palindrome s =
    let len = String.length s - 1 in
    let rec aux i j =
        if j <= i then
            true
        else if i > len || j < 0 || s.[i] <> s.[j] then
            false
        else
            aux (i + 1) (j - 1)
    in
    aux 0 len


let print_bool = function
    | true -> print_endline "true"
    | false -> print_endline "false"

let () =
    print_bool (ft_is_palindrome "radar");
    print_bool (ft_is_palindrome "madam");
    print_bool (ft_is_palindrome "car");
    print_bool (ft_is_palindrome "");
