type radar = float array * string

let eu_dist a b =
    let x = ref 0. in
    for i = 0 to Array.length a - 1 do
        let tmp = a.(i) -. b.(i) in
        x := !x +. tmp *. tmp
    done;
    !x ** 0.5

let one_nn (lst : radar list) ((base_tab, base_s as base) : radar) =
    let dist = ref infinity
    in
    let fct (tab, s) (tab', s') =
        let eu = eu_dist base_tab tab' in
        if eu < !dist then begin
            dist := eu;
            (tab', s')
        end
        else (tab, s)
    in
    let _, s = List.fold_left fct base lst in
    s

let k_nn (lst : radar list) k ((base_tab, base_s) : radar) =
        let tmp = List.sort (fun (tab, _) (tab', _) ->
                                let a = eu_dist base_tab tab
                                and b = eu_dist base_tab tab' in
                                if a > b then 1
                                else if a < b then -1
                                else 0) lst
        in
        let i = ref 0
        in
        let tmp = List.fold_left (fun a b -> if !i < k then (i := !i + 1; b :: a) else a) [] tmp
        in
        List.fold_left (fun a (_, b) -> a ^ b) "" tmp

let _ =
        let lst = [
                ([| 1.; |], "toto");
                ([| 20.; |], "tata");
                ([| 300.; |], "titi");
                ([| 4000.; |], "tutu");
        ]
        in
        print_endline (one_nn lst ([| 5.; |], "hello"));
        print_endline (one_nn lst ([| 30.; |], "hello"));
        print_endline (one_nn lst ([| 500.; |], "hello"));
        print_endline (one_nn lst ([| 6000.; |], "hello"));

        print_endline "--------------";

        let a = [
                ([| 1.; 2.; 3.; 4.; |], "toto");
                ([| 2.; 3.; 4.; 5.; |], "tata");
                ([| 3.; 4.; 5.; 6.; |], "titi");
                ([| 4.; 5.; 6.; 7.; |], "tutu");
        ]
        in
        print_endline (one_nn a ([| -2.; -3.; -4.; -5.; |], "hello"));
        print_endline (one_nn a ([| 2.; 3.; 4.; 5.; |], "hello"));
        print_endline (one_nn a ([| 3.; 4.; 5.; 6.; |], "hello"));
        print_endline (one_nn a ([| 4.; 5.; 6.; 7.; |], "hello"));

        print_endline "--------------";

        print_endline (k_nn lst 4 ([| 6000.; |], "hello"));
        print_endline (k_nn lst 3 ([| 6000.; |], "hello"));
        print_endline (k_nn lst 2 ([| 6000.; |], "hello"));
        print_endline (k_nn lst 1 ([| 6000.; |], "hello"));
