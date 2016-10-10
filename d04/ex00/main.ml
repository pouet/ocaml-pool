let () =
    List.iter (fun x -> print_endline (Color.toString x)) Color.all;
    List.iter (fun x -> print_endline (Color.toStringVerbose x)) Color.all
