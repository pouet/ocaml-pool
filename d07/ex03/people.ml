class people name =
    object
        val _name : string = name
        val _hp : int = 100

        initializer print_endline "I'm aliiiiiiiiiiveeeee !!!!!"

        method to_string = _name ^ " : " ^ (string_of_int _hp)
        method talk = print_endline ("I'm " ^ _name ^ " ! Do you know the Doctor ?")
        method die = print_endline "Aaaarghh !"
    end
