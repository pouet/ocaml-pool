let _ =
    Random.self_init ();

    let a = new Doctor.doctor "toto" 50 in
    let b = new People.people "titi" in

    let d = new Dalek.dalek in

    print_endline d#to_string;
    d#talk;
    d#exterminate b;
    print_endline d#to_string;
    d#exterminate b;
    print_endline d#to_string;
    d#die
