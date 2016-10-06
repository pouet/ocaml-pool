let encode l =
    let rec aux fst n = function
        | [] -> []
        | hd :: [] ->
                if fst == hd then (n + 1, fst) :: []
                else (n, fst) :: (1, hd) :: []
        | hd :: tl ->
                if fst == hd then aux fst (n + 1) tl
                else (n, fst) :: (aux hd 1 tl)
    in
    let f = function
        | [] -> []
        | hd :: tl -> aux hd 0 l
    in
    f l

let print_list l =
    let rec aux = function
        | [] -> ()
        | (x , _) :: tl ->
                print_int x;
                print_endline "";
                aux tl
    in
    aux l


let () =
    let l = [ 5; 5; 5; 3; 1; 1; 1; ] in
    print_list (encode l)
