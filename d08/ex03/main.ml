let _ =
    let h = new Atom.hydrogen in
    let he = new Atom.helium in
    let li = new Atom.lithium in
    let be = new Atom.beryllium in
    let b = new Atom.bore in
    let c = new Atom.carbon in
    let n = new Atom.nitrogen in
    let o = new Atom.oxygen in

    print_endline h#to_string;
    print_endline he#to_string;
    print_endline li#to_string;
    print_endline be#to_string;
    print_endline b#to_string;
    print_endline c#to_string;
    print_endline n#to_string;
    print_endline o#to_string;

    print_endline "-------------";

    Printf.printf "equals H He : %B\n" (h#equals he);
    Printf.printf "equals Li Li : %B\n" (li#equals li);
    Printf.printf "equals Be B : %B\n" (be#equals b);
    Printf.printf "equals c c : %B\n" (c#equals c);
    Printf.printf "equals n o : %B\n" (n#equals o);

    let w = new Molecule.water in
    let tnt = new Molecule.trinitrotoluene in
    let cd = new Molecule.carbon_dioxyde in
    let dm = new Molecule.dicarbon_monoxide in
    let oz = new Molecule.ozone in
    let fa = new Molecule.fulminic_acid in
    let mth = new Molecule.methan in

    print_endline w#to_string;
    print_endline tnt#to_string;
    print_endline cd#to_string;
    print_endline dm#to_string;
    print_endline oz#to_string;
    print_endline fa#to_string;
    print_endline mth#to_string;

    print_endline "-------------";

    Printf.printf "equals water water : %B\n" (w#equals w);
    Printf.printf "equals tnt carbon_dioxyde : %B\n" (tnt#equals cd);
    Printf.printf "equals carbon_dioxyde ozone : %B\n" (cd#equals oz);
    Printf.printf "equals dicarbon_monoxide methane : %B\n" (dm#equals mth);
    Printf.printf "equals fulminic_acid fulminic_acid : %B\n" (fa#equals fa);

    print_endline "-------------";

    let methane = new Alkane.methane in
    let ethane = new Alkane.ethane in
    let propane = new Alkane.propane in
    let butane = new Alkane.butane in
    let pentane = new Alkane.pentane in
    let hexane = new Alkane.hexane in
    let heptane = new Alkane.heptane in
    let octane = new Alkane.octane in
    let nonane = new Alkane.nonane in
    let decane = new Alkane.decane in
    let undecane = new Alkane.undecane in
    let dodecane = new Alkane.dodecane in

    print_endline methane#to_string;
    print_endline ethane#to_string;
    print_endline propane#to_string;
    print_endline butane#to_string;
    print_endline pentane#to_string;
    print_endline hexane#to_string;
    print_endline heptane#to_string;
    print_endline octane#to_string;
    print_endline nonane#to_string;
    print_endline decane#to_string;
    print_endline undecane#to_string;
    print_endline dodecane#to_string;

    print_endline "-------------";

    Printf.printf "equals methane methane : %B\n" (methane#equals methane);
    Printf.printf "equals ethane pentane : %B\n" (ethane#equals pentane);
    Printf.printf "equals dodecane dodecane : %B\n" (dodecane#equals dodecane)
