class [ 'a, 'b, 'c ] galifrey (dalek : 'a) (doctor : 'b) (people : 'c) =
    object
        val _dalek = dalek
        val _doctor = doctor
        val _people = people

        method do_time_war =
            _people#print_all;
            _people#delete;
            _people#delete;
            _people#delete;
            _people#delete;
            _people#print_all;

            _doctor#print_all;
            _doctor#delete;
            _doctor#delete;
            _doctor#delete;
            _doctor#delete;
            _doctor#print_all;

            _dalek#print_all;
            _dalek#delete;
            _dalek#delete;
            _dalek#delete;
            _dalek#delete;
            _dalek#print_all
    end
