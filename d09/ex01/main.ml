let print_proj ((s, t, n) : App.project) =
    Printf.printf "%s - %s - %d\n" s t n

let _ =
    let a = App.zero
    and b = App.fail ("toto", "succeed", 12)
    and c = App.succeed ("tata", "failed", 32) in
    let d = App.combine b c
    and e = App.combine ("tutu ", "ole", 42) ("titi", "ahah", 42) in

    print_proj a;
    print_proj b;
    print_proj c;
    print_proj d;
    print_proj e
