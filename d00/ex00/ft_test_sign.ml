let ft_test_sign n =
    if n < 0 then print_endline "negative"
    else print_endline "positive"

(* ------------------------ *)
(* ------------------------ *)

let test n =
    print_string "Test with [";
    print_int n;
    print_string "]: ";
    ft_test_sign n

let () =
    test (-42);
    test 0;
    test 42
