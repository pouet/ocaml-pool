type radar = float array * string

let eu_dist a b =
    let x = ref 0. in
    for i = 0 to Array.length a - 1 do
        let tmp = a.(i) -. b.(i) in
        x := tmp *. tmp
    done;
    !x ** 0.5

let get_line f : radar =
    let l = input_line f in
    let s = Str.split (Str.regexp ",") l in
    let s = List.rev s in
    let c = List.hd s in
    if String.length c <> 1 then failwith ("Invalid character : " ^ c);
    let s = List.rev (List.tl s) in
    let s = List.map (fun s -> if String.length s = 0 then failwith ("Invalid number : " ^ s); float_of_string s) s in
    let tab = Array.of_list s in
    (tab, c)

let examples_of_file f =
    let in_file = open_in f in
    let ret = ref [] in
    let get_input () =
        try
            while true do
                let l = get_line in_file in
                ret := l :: !ret
            done
        with
        | End_of_file   -> close_in in_file
        | Failure err   -> close_in in_file; ret := []; print_endline err
        | _             -> ret := []; print_endline "Invalid file format"
    in
    get_input ();
    List.rev !ret

let one_nn (lst : radar list) (base : radar) =
    let dist = ref infinity
    in
    let fct (tab, s) (tab', s') =
        let eu = eu_dist tab tab' in
        if String.compare s s' = 0 && eu < !dist then begin
            dist := eu;
            (tab, s)
        end
        else (tab', s')
    in
    let _, s = List.fold_left fct base lst in
    s

let _ =
(*
    let f = "examples_of_file.ml" in
    let f = "ionosphere.test.csv" in
*)
    let f = "ionosphere.train.csv" in
    let r = examples_of_file f in
    match r with
    | hd :: tl  -> print_endline (one_nn tl hd)
    | _         -> ()
