(* ocaml str.cma examples_of_file.ml *)

let get_line f =
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

let _ =
    let f = "ionosphere.train.csv" in
    let f = "examples_of_file.ml" in
    let f = "ionosphere.test.csv" in
    let r = examples_of_file f in
    List.iter (fun r -> 
        Array.iter (fun f -> Printf.printf "%f " f) (fst r);
        print_endline (snd r)) r;
    Printf.printf "%d\n" (List.length r)
