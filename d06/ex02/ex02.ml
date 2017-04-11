module type PAIR = sig
    val pair : (int * int)
end

module type VAL = sig
    val x : int
end

module type MAKEPARAM =
    functor (Val : PAIR) -> VAL

module MakeFst : MAKEPARAM =
    functor (Val : PAIR) ->
        struct
            let x = fst Val.pair
        end

module MakeSnd : MAKEPARAM =
    functor (Val : PAIR) ->
        struct
            let x = snd Val.pair
        end

module Pair : PAIR = struct
    let pair = ( 21, 42 )
end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
