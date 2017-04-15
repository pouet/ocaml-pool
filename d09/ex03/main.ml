let test s a =
    Printf.printf "%9s : %s\n" s (Try.to_string a)

let _ =
    let a = Try.return "toto" in
    let b = Try.bind a (fun _ -> invalid_arg "toto") in
    let c = Try.bind a (fun x -> x) in
    let d = Try.recover b (fun _ -> Try.return "recover ok") in
    let e = Try.recover c (fun _ -> Try.return "recover nok") in
    let f = Try.filter b (fun _ -> false) in
    let g = Try.filter c (fun _ -> true) in
    let h = Try.flatten (Try.return (Try.return "toto")) in
    let i = Try.flatten (Try.return (Try.Failure Try.Fail)) in
    let j = Try.flatten (Try.Failure Try.Fail) in

    test "return" a;
    test "bind" b;
    test "bind" c;
    test "recover" d;
    test "recover" e;
    test "filter" f;
    test "filter" g;
    test "flatten" h;
    test "flatten" i;
    test "flatten" j
