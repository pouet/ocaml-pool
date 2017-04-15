let test s l =
    let pf = Printf.printf in

    pf "%7s: " s;
    Set.foreach l (fun n -> pf "%d " n);
    print_endline ""

let _ =
(*     let a = Set.return 21 *)
    let c = Set.return 42 in
(*     let c = Set.union a b in *)
    let c = Set.union c (Set.return (42 * 2)) in
    let c = Set.union c (Set.return (42 * 3)) in
    let c = Set.union c (Set.return (42 * 4)) in
    let c = Set.union c (Set.return (42 * 5)) in
    let d = Set.bind c (fun n : 'a Set.t -> [ n * 2 ]) in
    let e = Set.inter c d in
    let f = Set.diff c d in
    let g = Set.filter c (fun n -> n mod 3 = 0) in
    Printf.printf "for_all: %B\n" (Set.for_all c (fun n -> n = 42));
    Printf.printf "for_all: %B\n" (Set.for_all c (fun n -> n mod 2 = 0));
    Printf.printf " exists: %B\n" (Set.exists c (fun n -> n = 42));

    test "union" c;
    test "bind" d;
    test "inter" e;
    test "diff" f;
    test "filter" g
