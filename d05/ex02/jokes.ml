let () =
    let jokes = [|
        "C'est l'histoire d'un gars qui a 5 bites. Son slip lui va comme un gant";
        "J'ai cru que Mozart était mort mais mozzarella";
        "Ce matin, j'ai voulu faire une blague sur le Super U, mais elle n'a pas Supermarché !";
        "C'est l'histoire d'un Schtroumpf qui court, il tombe et se fait un bleu !";
        "C'est l'histoire d'un pinguouin qui respire par le cul. Un jour il s'assoit et il meurt !";
    |] in
    Random.self_init ();
    let n = Random.int (Array.length jokes) in
    print_endline jokes.(n)
