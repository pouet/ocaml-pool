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
    Random.self_init ();
    let jokes = get_jokes "jokes.txt" in
    let n = Random.int (Array.length jokes) in
    print_endline jokes.(n)
