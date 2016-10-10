let () =
    List.iter (fun x -> print_endline (string_of_int (Value.toInt x))) Value.all;
    List.iter (fun x -> print_endline (Value.toString x)) Value.all;
    List.iter (fun x -> print_endline (Value.toStringVerbose x)) Value.all;
    List.iter (fun x -> print_endline (Value.toStringVerbose (Value.next x))) Value.all;
    List.iter (fun x -> print_endline (Value.toStringVerbose (Value.previous x))) Value.all
