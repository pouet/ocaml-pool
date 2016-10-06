let sequence n =
    let getnext n =
        let rec aux n fst cnt =
            if n == 0 then (cnt * 10) + fst
            else begin
                let tmp = n mod 10 in
                if tmp == fst then aux (n / 10) fst (cnt + 1)
                else ((cnt * 10) + fst) + 100 * (aux (n / 10) tmp 1)
            end
        in
        aux n (n mod 10) 0
    in
    let rec loop i x =
        if i >= n then string_of_int x
        else loop (i + 1) (getnext x)
    in
    if n < 0 then ""
    else loop 0 1

let () =
    for i = -1 to 6 do
        print_endline (sequence i)
    done
