class doctor name age =
    object
        val _name : string = name
        val _age : int = age
        val _sidekick = new People.people name
        val _hp : int = 50

        initializer print_endline "The Doctor is aliiiiiiiiiiveeeee !!!!!"

        method to_string =
            "Name : " ^ _name ^ " | Age : " ^ (string_of_int _age) ^ " | HP : "
            ^ (string_of_int _hp) ^ " | SideKick : " ^ _sidekick#to_string

        method talk =
            print_endline "Hi ! I'm the Doctor !"

        method use_sonic_screwdriver =
            print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

        method private regenerate =
            {< _hp = 100 >}

        method travel_in_time start arrival =
            let age = _age +  arrival - start in
            if age < 0 then begin
                print_endline "The doctor can't travel so far !";
                {< >}
            end
            else begin
                print_endline "          _";
                print_endline "         /-\\";
                print_endline "    _____|#|_____";
                print_endline "   |_____________|";
                print_endline "  |_______________|";
                print_endline "|||_POLICE_##_BOX_|||";
                print_endline " | |¯|¯|¯|||¯|¯|¯| |";
                print_endline " | |-|-|-|||-|-|-| |";
                print_endline " | |_|_|_|||_|_|_| |";
                print_endline " | ||~~~| | |¯¯¯|| |";
                print_endline " | ||~~~|!|!| O || |";
                print_endline " | ||~~~| |.|___|| |";
                print_endline " | ||¯¯¯| | |¯¯¯|| |";
                print_endline " | ||   | | |   || |";
                print_endline " | ||___| | |___|| |";
                print_endline " | ||¯¯¯| | |¯¯¯|| |";
                print_endline " | ||   | | |   || |";
                print_endline " | ||___| | |___|| |";
                print_endline "|¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|";
                print_endline " ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯";
                {< _age = age >}
            end
    end
