let dalek_name () =
    let tab = [|
        "DalekSec";
        "DalekCaan";
        "DalekThay";
        "DalekJast";
    |] in
    tab.(Random.int 4)

class dalek =
    object (self)
        val _name : string = dalek_name ()
        val _hp : int = 100
        val _shield : bool ref = ref true

(*         initializer  *)

        method to_string =
            "Name : " ^ _name ^ " | HP : " ^ (string_of_int _hp) ^ " | Shield : " ^
            (if !_shield then "true" else "false")

        method talk =
            let tab = [|
                "Explain! Explain!";
                "Exterminate! Exterminate!";
                "I obey!";
                "You are the Doctor! You are the enemy of the Daleks!";
            |] in
            print_endline tab.(Random.int 4)

        method exterminate (ppl : People.people) =
            ppl#die;
            _shield := (if !_shield = true then false else true)

        method die =
            print_endline "Emergency Temporal Shift !"
    end
