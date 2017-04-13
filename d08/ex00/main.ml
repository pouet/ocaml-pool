let _ =
    let h = new Atom.hydrogen in
    let he = new Atom.helium in
    let li = new Atom.lithium in
    let be = new Atom.beryllium in
    let b = new Atom.bore in
    let c = new Atom.carbon in
    let n = new Atom.azot in
    let o = new Atom.oxygen in

    print_endline h#to_string;
    print_endline he#to_string;
    print_endline li#to_string;
    print_endline be#to_string;
    print_endline b#to_string;
    print_endline c#to_string;
    print_endline n#to_string;
    print_endline o#to_string;

    print_endline "-------------";

    Printf.printf "equals H He : %B\n" (h#equals he);
    Printf.printf "equals Li Li : %B\n" (li#equals li);
    Printf.printf "equals Be B : %B\n" (be#equals b);
    Printf.printf "equals c c : %B\n" (c#equals c);
    Printf.printf "equals n o : %B\n" (n#equals o)
