let crossover l1 l2 =
    let rec find x = function
        | [] -> false
        | hd :: tl ->
                if hd == x then true
                else find x tl
    in
    let rec cross = function
        | [] -> []
        | hd :: tl ->
                if find hd l1 then hd :: cross tl
                else cross tl
    in
    cross l2

let rec print_list = function
    | [] -> print_endline "[]";
    | hd :: tl ->
            print_int hd;
            print_string " :: ";
            print_list tl

let () =
    let m = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; ] in
    let l = [ 2; 4; 7; 9; ] in
    print_list (crossover l m)
