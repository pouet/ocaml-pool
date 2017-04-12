let gen_dalek =
    let rec gen = function
        | 0     -> []
        | i     -> new Dalek.dalek :: (gen (i - 1))
    in
    gen (Random.int 10 + 1)

let gen_doctor =
    let rec gen = function
        | 0     -> []
        | i     -> new Doctor.doctor ("doctor" ^ (string_of_int i))
                                    (Random.int 90 + 10) :: (gen (i - 1))
    in
    gen (Random.int 10 + 1)

let gen_people =
    let rec gen = function
        | 0     -> []
        | i     -> new People.people ("people" ^ (string_of_int i)) :: (gen (i - 1))
    in
    gen (Random.int 10 + 1)

class galifrey =
    object (self)
        val _dalek = gen_dalek
        val _doctor = gen_doctor
        val _people = gen_people

        method do_time_war () =
            ()
    end
