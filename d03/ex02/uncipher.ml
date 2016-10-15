let uncaesar n s =
    let code = int_of_char
    and chr = char_of_int in
    let _a = code 'a'
    and _A = code 'A' in
    let rot c =
        let a = code c in
        if c >= 'a' && c <= 'z' then
            chr ((a - _a - n) mod 26 + _a)
        else if c >= 'A' && c <= 'Z' then
            chr ((a - _A - n) mod 26 + _A)
        else
            c
    in
    String.map rot s

let unrot42 = uncaesar 42 

let xor n s =
    String.map (fun c -> char_of_int ((int_of_char c) lxor n)) s

let rec ft_uncrypt (s : string) = function
    | [] -> s
    | hd :: tl -> ft_uncrypt (hd s) tl

let () =
    print_endline (ft_uncrypt "salutations" [ unrot42; (uncaesar 13); (xor 7); ]);
    print_endline (unrot42 "salutations");
    print_endline (xor 13 "salutations")

