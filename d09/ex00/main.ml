let _ =
    let z = Watchtower.zero in

    for i = -24 to 24 do
        Printf.printf "%d " (Watchtower.add z (i : Watchtower.hour))
    done;

    print_endline "";
    print_endline "----------";

    for i = 24 downto -24 do
        Printf.printf "%d " (Watchtower.sub z (i : Watchtower.hour))
    done;
    print_endline ""
