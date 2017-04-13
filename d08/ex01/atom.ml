class virtual atom n sym num =
    object (self : 'self)
        method name = n
        method symbol = sym
        method number = num

        method to_string = "Atom: " ^ self#name ^ " " ^ (string_of_int self#number) ^
            " " ^ self#symbol
        method equals (other : 'self) = other#number = self#number
    end

class hydrogen =
    object
        inherit atom "hydrogen" "H" 1
    end

class helium =
    object
        inherit atom "helium" "He" 2
    end

class lithium =
    object
        inherit atom "lithium" "Li" 3
    end

class beryllium =
    object
        inherit atom "beryllium" "Be" 4
    end

class bore =
    object
        inherit atom "bore" "B" 5
    end

class carbon =
    object
        inherit atom "carbon" "C" 6
    end

class nitrogen =
    object
        inherit atom "nitrogen" "N" 7
    end

class oxygen =
    object
        inherit atom "oxygen" "O" 8
    end
