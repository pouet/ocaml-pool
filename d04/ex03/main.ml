let _ =
        Random.self_init ();
        List.iter (fun x -> print_endline x) (Deck.toStringList (Deck.newDeck ()));
        List.iter (fun x -> print_endline x) (Deck.toStringListVerbose (Deck.newDeck ()));
        print_endline " --- - --- - - - - -";
        print_endline (Deck.Card.toStringVerbose (Deck.Card.newCard Deck.Card.Value.Queen Deck.Card.Color.Heart));
        let c, _ = Deck.drawCard (Deck.newDeck ()) in
        print_endline (Deck.Card.toStringVerbose c)
