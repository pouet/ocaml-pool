type phosphate = string
type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide = function
    | 'A' -> "phosphate", "deoxyribose", A
    | 'T' -> "phosphate", "deoxyribose", T
    | 'C' -> "phosphate", "deoxyribose", C
    | 'G' -> "phosphate", "deoxyribose", G
    | _ -> "phosphate", "deoxyribose", None

let test c =
    let tmp = generate_nucleotide c in
    (fun (p, d, _) -> print_string p; print_string " : "; print_string d) tmp;
    print_endline ""

let () =
    test 'C';
    test 'G';
    test 'T';
    test 'd';
