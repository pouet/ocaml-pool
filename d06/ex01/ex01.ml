module StringHashtbl = Hashtbl.Make(struct
        type t = string

        let equal = (=)
        let hash s =
            let len = String.length s - 1 in
            let rec aux acc = function
                | 0 | -1    -> acc
                | n         -> aux ((acc * 54059) lxor (Char.code s.[n] * 76963)) (n - 1)
            in
            aux 37 len
    end)

let () =
    let ht = StringHashtbl.create 5 in
    let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
    let pairs = List.map (fun s -> (s, String.length s)) values in
    List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
    StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
