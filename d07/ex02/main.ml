let _ =
    Random.self_init ();

    let ppl = new People.people "titi" in
    let dct = new Doctor.doctor "toto" 50 in
    let dal = new Dalek.dalek in

    print_endline ppl#to_string;
    print_endline dct#to_string;
    print_endline dal#to_string;

    ppl#talk;
    dct#talk;
    dal#talk;

    dal#exterminate ppl;
    print_endline dal#to_string;
    dal#exterminate ppl;
    print_endline dal#to_string;

    ppl#die;
    dal#die
