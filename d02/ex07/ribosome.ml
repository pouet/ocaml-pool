type phosphate = string
type deoxyribose = string

type nucleobase = A | T | C | G | U | None
type rna = nucleobase list

type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list

type aminoacid =
    | Stop
    | Ala
    | Arg
    | Asn
    | Asp
    | Cys
    | Gln
    | Glu
    | Gly
    | His
    | Ile
    | Leu
    | Lys
    | Met
    | Phe
    | Pro
    | Ser
    | Thr
    | Trp
    | Tyr
    | Val

type protein = aminoacid list

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

let generate_bases_triplets (r : rna) =
    let rec aux = function
        | a :: b :: c :: tl -> (a, b, c) :: aux tl
        | _ -> []
    in
    aux r

let string_of_protein (prot : protein) =
    let get_str = function
        | Stop -> "End of translation\n"
        | Ala -> "Alanine - "
        | Arg -> "Arginine - "
        | Asn -> "Asparagine - "
        | Asp -> "Aspartique - "
        | Cys -> "Cysteine - "
        | Gln -> "Glutamine - "
        | Glu -> "Glutamique - "
        | Gly -> "Glycine - "
        | His -> "Histidine - "
        | Ile -> "Isoleucine - "
        | Leu -> "Leucine - "
        | Lys -> "Lysine - "
        | Met -> "Methionine - "
        | Phe -> "Phenylalanine - "
        | Pro -> "Proline - "
        | Ser -> "Serine - "
        | Thr -> "Threonine - "
        | Trp -> "Tryptophane - "
        | Tyr -> "Tyrosine - "
        | Val -> "Valine - "
    in
    let rec aux = function
        | [] -> ""
        | hd :: tl -> get_str hd ^ aux tl
    in
    aux prot

let decode_arn (r : rna) =
    let get_amino = function
        | U, A, A | U, A, G | U, G, A -> Stop
        | G, C, A | G, C, C | G, C, G | G, C, U -> Ala
        | A, G, A | A, G, G | C, G, A | C, G, C | C, G, G | C, G, U -> Arg
        | A, A, C | A, A, U -> Asn
        | G, A, C | G, A, U -> Asp
        | U, G, C | U, G, U -> Cys
        | C, A, A | C, A, G -> Gln
        | G, A, A | G, A, G -> Glu
        | G, G, A | G, G, G | G, G, U -> Gly
        | C, A, C | C, A, U -> His
        | A, U, A | A, U, C | A, U, U -> Ile
        | C, U, A | C, U, C | C, U, G | C, U, U | U, U, A | U, U, G -> Leu
        | A, A, A | A, A, G -> Lys
        | A, U, G -> Met
        | U, U, C | U, U, U -> Phe
        | C, C, C | C, C, A | C, C, G | C, C, U -> Pro
        | U, C, A | U, C, C | U, C, G | U, C, U | A, G, U | A, G, C -> Ser
        | A, C, A | A, C, C | A, C, G | A, C, U -> Thr
        | U, G, G -> Trp
        | U, A, C | U, A, U -> Tyr
        | G, U, A | G, U, C | G, U, G | G, U, U -> Val
        | _ -> Stop
    in
    let rec aux = function
        | hd :: tl ->
                let tmp = get_amino hd in
                if tmp <> Stop then tmp :: aux tl
                else Stop :: []
        | _ -> Stop :: []
    in
    (aux (generate_bases_triplets r) : protein)

let _ =
    let h = generate_helix 10 in
    let r = generate_rna h in
    let arn = decode_arn r in 
    print_endline (string_of_protein arn)
