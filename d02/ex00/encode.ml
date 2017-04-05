let encode l =
    let rec aux fst n = function
        | [] -> (n, fst) :: []
        | hd :: tl ->
                if fst == hd then aux fst (n + 1) tl
                else (n, fst) :: (aux hd 1 tl)
    in
    match l with
    | [] -> []
    | hd :: _ -> aux hd 0 l


let print_list l =
    let rec aux = function
        | [] -> ()
        | (x , n) :: tl ->
                Printf.printf "%d-%d\n" x n;
                aux tl
    in
    aux l

let () =
    let l = [ 5; 5; 5; 3; 1; 1; 1; 2; ] in
    print_list (encode l)
