let _ =
    let a = new Doctor.doctor "toto" 50 in

    print_endline a#to_string;
    a#talk;
    a#use_sonic_screwdriver;
    let a = a#travel_in_time 2017 1987 in
    print_endline a#to_string;
    let a = a#travel_in_time 1987 100 in
    print_endline a#to_string
