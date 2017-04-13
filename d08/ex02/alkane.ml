let get_alkanes_mol n =
    let rec aux_car i = function
        | 0     -> []
        | n     -> new Atom.carbon :: aux_car i (n - 1)
    in
    let rec aux_hyd i = function
        | 0     -> []
        | n     -> new Atom.hydrogen :: aux_hyd i (n - 1)
    in
    Molecule.string_of_atoms ((aux_car 0 n) @ (aux_hyd 1 (n * 2 + 2)))

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
    object (self : 'self)
        method name = get_alkane_name n
        method forumula = get_alkanes_mol n

        method to_string = "Alkane: " ^ self#name ^ " " ^ self#forumula
        method equals (other : 'self) = other#forumula = self#forumula
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
