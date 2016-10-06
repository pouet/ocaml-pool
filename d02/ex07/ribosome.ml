type phosphate = string
type deoxyribose = string

type nucleobase = A | T | C | G | U | None
type rna = nucleobase list

type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list

let generate_nucleotide = function
    | 'A' -> "phosphate", "deoxyribose", A
    | 'T' -> "phosphate", "deoxyribose", T
    | 'C' -> "phosphate", "deoxyribose", C
    | 'G' -> "phosphate", "deoxyribose", G
    | 'U' -> "phosphate", "deoxyribose", U
    | _ -> "phosphate", "deoxyribose", None

let generate_helix n =
    Random.self_init ();
    let s = "ATCGU?" in

    let rec aux = function
        | 0 -> []
        | n -> generate_nucleotide s.[Random.int 5] :: aux (n - 1)
    in
    aux n

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
    aux h

let generate_rna (h : helix) =
    let get_pair = function
        | A -> U
        | T -> A
        | C -> G
        | G -> C
        | _ -> None
    in
    let rec aux = function
        | [] -> []
        | (_, _, b) :: tl -> get_pair b :: aux tl
    in
    aux h

let generate_bases_triplets r =
    let rec aux = function
        | (_, _, b) :: (_, _, c) :: (_, _, d) :: tl -> b * c * d :: aux tl
        | _ -> []
    in
    aux r

TODO protein - decode_arm
