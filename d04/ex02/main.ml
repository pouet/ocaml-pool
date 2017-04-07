let print_bool = function
    | true  -> print_endline "true"
    | false -> print_endline "false"

let _ =
    let c1 = Card.newCard Card.Value.T5 Card.Color.Spade
    and c2 = Card.newCard Card.Value.Queen Card.Color.Club
    and c3 = Card.newCard Card.Value.As Card.Color.Heart
    in
    List.iter (fun e -> print_endline (Card.toString e)) Card.all;
    List.iter (fun e -> print_endline (Card.toStringVerbose e)) Card.all;
    print_endline "------------";
    print_endline (Card.toStringVerbose (Card.min c1 c2));
    print_endline (Card.toStringVerbose (Card.max c1 c2));
    Printf.printf "%d\n" (Card.compare c1 c2);
    print_endline (Card.toStringVerbose (Card.best [ c2; c3; c1 ]));
    print_bool (Card.isDiamond c1);
    print_bool (Card.isSpade c1)
