let _ =
        let a = Try.return "toto" in

        print_endline (Try.to_string a)
