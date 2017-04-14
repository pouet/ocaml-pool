module type MONOID =
    sig
        type element
        val zero1 : element
        val zero2 : element
        val mul : element -> element -> element
        val add : element -> element -> element
        val div : element -> element -> element
        val sub : element -> element -> element
    end

module INT : (MONOID with type element = int) =
    struct
        type element = int

        let zero1 = 0
        let zero2 = 1

        let add = ( + )
        let sub = ( - )
        let mul = ( * )
        let div = ( / )
    end

module FLOAT : (MONOID with type element = float) =
    struct
        type element = float

        let zero1 = 0.
        let zero2 = 0.

        let add = ( +. )
        let sub = ( -. )
        let mul = ( *. )
        let div = ( /. )
    end

module type MAKECALC =
    functor (M : MONOID) ->
        sig
            type element
            val add : M.element -> M.element -> M.element
            val sub : M.element -> M.element -> M.element
            val mul : M.element -> M.element -> M.element
            val div : M.element -> M.element -> M.element
            val power : M.element -> int -> M.element
            val fact : M.element -> M.element
        end with type element := M.element

module Calc =
    functor (M : MONOID) ->
        struct
            let add = M.add
            let sub = M.sub
            let mul = M.mul
            let div = M.div

            let power el n =
                let rec aux acc = function
                    | 0 | 1         -> acc
                    | n when n > 0  -> aux (M.mul el acc) (n - 1)
                    | _             -> invalid_arg "power can't be negative"
                in
                aux el n

            let fact el =
                let one = M.div el el in
                let rec aux acc n =
                    if n <= el then aux (M.mul n acc) (M.add n one)
                    else acc
                in
                aux one one
        end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let _ =
    Printf.printf "add 21 21: %d\n" (Calc_int.add 21 21);
    Printf.printf "sub 21 21: %d\n" (Calc_int.sub 21 21);
    Printf.printf "mul 21 10: %d\n" (Calc_int.mul 21 10);
    Printf.printf "div 21 21: %d\n" (Calc_int.div 21 21);
    Printf.printf "pow 3 3  : %d\n" (Calc_int.power 3 3);
    Printf.printf "fac 5    : %d\n" (Calc_int.fact 5);

    print_endline "------------------------";

    Printf.printf "add 21 21: %f\n" (Calc_float.add 21. 21.);
    Printf.printf "sub 21 21: %f\n" (Calc_float.sub 21. 21.);
    Printf.printf "mul 21 10: %f\n" (Calc_float.mul 21. 10.);
    Printf.printf "div 21 21: %f\n" (Calc_float.div 21. 21.);
    Printf.printf "pow 3 3  : %f\n" (Calc_float.power 3. 3);
    Printf.printf "fac 5    : %f\n" (Calc_float.fact 5.)
