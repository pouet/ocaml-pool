let _ =
    Random.self_init ();

    let army = new Army.army in
    army#add (new People.people "toto");
    army#add (new People.people "titi");
    army#add (new People.people "tutu");
    army#add (new People.people "tata");
    army#print_all;
    army#delete;
    army#delete;
    army#delete;
    army#delete;
    army#print_all;

    print_endline "---------------------";

    let army = new Army.army in
    army#add (new Doctor.doctor "toto" 10);
    army#add (new Doctor.doctor "titi" 20);
    army#add (new Doctor.doctor "tutu" 30);
    army#add (new Doctor.doctor "tata" 40);
    army#print_all;
    army#delete;
    army#delete;
    army#delete;
    army#delete;
    army#print_all;

    print_endline "---------------------";

    let army = new Army.army in
    army#add (new Dalek.dalek);
    army#add (new Dalek.dalek);
    army#add (new Dalek.dalek);
    army#add (new Dalek.dalek);
    army#print_all;
    army#delete;
    army#delete;
    army#delete;
    army#delete;
    army#print_all
