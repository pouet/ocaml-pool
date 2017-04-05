let gray n =
    let print_bin x =
        let rec loop x = function
            | 0 -> ()
            | n ->
                    loop (x / 2) (n - 1);
                    print_int (x mod 2);
        in
        loop x n;
        print_string " "
    in
    let rec aux i =
        if i < 1 lsl n then begin
            print_bin (i lxor (i lsr 1));
            aux (i + 1)
        end
        else () 
    in
    if n > 0 then aux 0;
    print_endline ""

let () =
    gray 1;
    gray 2;
    gray 3
