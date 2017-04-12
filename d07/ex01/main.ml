let _ =
    let a = new Doctor.doctor "toto" 50 in

    print_endline a#to_string;
    a#talk;
    a#use_sonic_screwdriver;
    print_endline (a#travel_in_time 2017 1987)#to_string
