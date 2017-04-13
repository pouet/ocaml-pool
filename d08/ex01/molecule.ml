(* Carbon first -> Hidrogen second -> rest in alphabetical order *)

let string_of_atoms at =
    let c_exists = List.exists (fun x -> x#symbol = "C") at in
    let rec aux n = function
        | []                -> ""
        | hd :: (nxt :: _ as tl)   -> if hd#symbol = nxt#symbol then aux (n + 1) tl
                                else hd#symbol ^ (string_of_int n) ^ (aux 1 tl)
        | hd :: tl          -> hd#symbol ^ (string_of_int n) ^ (aux 1 tl)
    in
    aux 1 (List.sort (fun x y -> match x#symbol, y#symbol with
                    | "C"   , _                 -> -1
                    | _     , "C"               -> 1
                    | "H"   , _   when c_exists -> -1
                    | _     , "H" when c_exists -> 1
                    | s     , t                 -> if s > t then 1 else -1) at)

class virtual molecule n form =
    object (self)
        method name = n
        method forumula = string_of_atoms form

        method to_string = "Molecule: " ^ self#name ^ " -> " ^ self#forumula
    end

class water =
    object
        inherit molecule "water" [ new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen; ]
    end

class trinitrotoluene =
    object
        inherit molecule "trinitrotoluene" [
            new Atom.nitrogen; new Atom.nitrogen; new Atom.nitrogen;
            new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
            new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen;
            new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
        ]
    end

class carbon_dioxyde =
    object
        inherit molecule "carbon dioxyde" [ new Atom.carbon; new Atom.oxygen; new Atom.oxygen; ]
    end

class dicarbon_monoxide =
    object
        inherit molecule "dicarbon monoxide" [ new Atom.carbon; new Atom.carbon; new Atom.oxygen; ]
    end

class ozone =
    object
        inherit molecule "ozone" [ new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; ]
    end

class fulminic_acid =
    object
        inherit molecule "fulminic acid" [ new Atom.hydrogen; new Atom.carbon; new Atom.nitrogen; new Atom.oxygen; ]
    end

class methan =
    object
        inherit molecule "methan" [ new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; ]
    end
