let _ =
    let a = new People.people "bob" in

    print_endline a#to_string;
    a#talk;
    a#die
