let eu_dist a b =
    let x = ref 0. in
    for i = 0 to Array.length a - 1 do
        let tmp = a.(i) -. b.(i) in
        x := tmp *. tmp
    done;
    !x ** 0.5

let () =
    print_float (eu_dist [| 1.; 2.; 3.; 4.; |] [| 4.; 3.; 2.; 1.; |]);
    print_endline ""
