type phosphate = string
type deoxyribose = string

type nucleobase = A | T | C | G | U | None
type rna = nucleobase list

type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list

let generate_nucleotide = function
    | 'A' -> ("phosphate", "deoxyribose", A : nucleotide)
    | 'T' -> ("phosphate", "deoxyribose", T : nucleotide)
    | 'C' -> ("phosphate", "deoxyribose", C : nucleotide)
    | 'G' -> ("phosphate", "deoxyribose", G : nucleotide)
    | 'U' -> ("phosphate", "deoxyribose", U : nucleotide)
    | _ ->   ("phosphate", "deoxyribose", None : nucleotide)

let generate_helix n =
    Random.self_init ();
    let s = "ATCGU?" in

    let rec aux = function
        | 0 -> []
        | n -> generate_nucleotide s.[Random.int 6] :: aux (n - 1)
    in
    (aux n : helix)

let helix_to_string (h : helix) =
    let nucleobase_to_string = function
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | U -> "U"
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
        | U -> 'U'
        | _ -> '?'
    in
    let rec aux = function
        | [] -> []
        | (_, _, b) :: tl ->
                generate_nucleotide (get_pair b) :: aux tl
    in
    (aux h : helix)

let generate_rna (h : helix) =
    let get_pair = function
        | A -> U
        | T -> A
        | C -> G
        | G -> C
        | U -> U
        | _ -> None
    in
    let rec aux = function
        | [] -> []
        | (_, _, b) :: tl -> get_pair b :: aux tl
    in
    (aux h : rna)


let rna_to_string (h : rna) =
    let nucleobase_to_string = function
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | U -> "U"
        | _ -> "?"
    in
    let rec aux = function
        | [] -> ""
        | hd :: tl -> (nucleobase_to_string hd) ^ aux tl
    in
    aux h

let _ =
    let h = generate_helix 10 in
    let r = generate_rna h in
    print_endline (helix_to_string h);
    print_endline (rna_to_string r)
