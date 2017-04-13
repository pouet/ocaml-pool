class virtual reaction start result =
    object
        method get_start : (Molecule.molecule * int) list
        method get_result : (Molecule.molecule * int) list
        method balance : reaction
        method is_balanced : bool
    end
