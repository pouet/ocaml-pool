let get_jokes file =
    let l = ref [] in
    try
        let f = open_in file in
        while true do
            let s = input_line f in
            l := s :: !l
        done;
        Array.of_list !l
    with
    | _ -> Array.of_list !l

let () =
        if (Array.length Sys.argv) == 2 then begin
            Random.self_init ();
            let jokes = get_jokes Sys.argv.(1) in
            let len = Array.length jokes in
            if len > 0 then begin
                    let n = Random.int len in
                    print_endline jokes.(n)
            end;
        end
