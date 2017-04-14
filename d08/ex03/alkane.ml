let get_alkanes_mol n =
    let rec aux f = function
        | 0     -> []
        | n     -> f () :: aux f (n - 1)
    in
    aux (fun () -> new Atom.carbon) n @ aux (fun () -> new Atom.hydrogen) (n * 2 + 2)

let get_alkane_name = function
    | 1     -> "methane"
    | 2     -> "ethane"
    | 3     -> "propane"
    | 4     -> "butane"
    | 5     -> "pentane"
    | 6     -> "hexane"
    | 7     -> "heptane"
    | 8     -> "octane"
    | 9     -> "nonane"
    | 10    -> "decane"
    | 11    -> "undecane"
    | 12    -> "dodecane"
    | _     -> invalid_arg "WTF MAN ?!"

class virtual alkane n =
    object
        inherit Molecule.molecule (get_alkane_name n) (get_alkanes_mol n)
    end

class methane = object inherit alkane 1 end
class ethane = object inherit alkane 2 end
class propane = object inherit alkane 3 end
class butane = object inherit alkane 4 end
class pentane = object inherit alkane 5 end
class hexane = object inherit alkane 6 end
class heptane = object inherit alkane 7 end
class octane = object inherit alkane 8 end
class nonane = object inherit alkane 9 end
class decane = object inherit alkane 10 end
class undecane = object inherit alkane 11 end
class dodecane = object inherit alkane 12 end
