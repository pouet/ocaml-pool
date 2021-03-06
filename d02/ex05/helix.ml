type phosphate = string
type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list

let generate_nucleotide = function
    | 'A' -> ("phosphate", "deoxyribose", A : nucleotide)
    | 'T' -> ("phosphate", "deoxyribose", T : nucleotide)
    | 'C' -> ("phosphate", "deoxyribose", C : nucleotide)
    | 'G' -> ("phosphate", "deoxyribose", G : nucleotide)
    | _ ->   ("phosphate", "deoxyribose", None : nucleotide)

let generate_helix n =
    Random.self_init ();
    let s = "ATCG?" in

    let rec aux = function
        | 0 -> []
        | n -> generate_nucleotide s.[Random.int 5] :: aux (n - 1)
    in
    (aux n : helix)

let helix_to_string (h : helix) =
    let nucleobase_to_string = function
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | _ -> "?"
    in
    let rec aux = function
        | [] -> ""
        | (_, _, t) :: tl -> (nucleobase_to_string t) ^ aux tl
    in
    aux h

let complementary_helix (h : helix) =
    let get_pair = function
        | A -> 'T'
        | T -> 'A'
        | C -> 'G'
        | G -> 'C'
        | _ -> '?'
    in
    let rec aux = function
        | [] -> []
        | (_, _, b) :: tl ->
                generate_nucleotide (get_pair b) :: aux tl
    in
    (aux h : helix)

let _ =
    let h = generate_helix 10 in
    let c = complementary_helix h in
    print_endline (helix_to_string h);
    print_endline (helix_to_string c)
