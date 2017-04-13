class [ 'a ] army =
    object
        val mutable lst : 'a list = [ ]

        method add soldier =
            lst <- soldier :: lst

        method delete =
            match lst with
            | []        -> ()
            | hd :: tl  -> lst <- tl

        method print_all =
            let rec print = function
                | []        -> ()
                | hd :: tl  -> begin
                    print tl;
                    print_endline hd#to_string
                end;
            in
            print lst
    end
