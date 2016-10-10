(* ocamlc unix.cma miconap.ml *)

let my_sleep () =
    Unix.sleep 1

let _ =
    if Array.length Sys.argv == 2 then begin
        try
            let n = int_of_string Sys.argv.(1) in
            for i = 1 to n do
                my_sleep ()
            done;
        with
        | _ -> ()
    end
