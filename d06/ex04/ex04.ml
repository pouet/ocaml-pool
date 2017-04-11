module type VAL = sig
    type t

    val add : t -> t -> t
    val mul : t -> t -> t
end

module type EVALEXPR = sig
    type t
    type expr =
        | Value of t
        | Add of expr * expr
        | Mul of expr * expr

    val eval : expr -> t
end

module type MAKEEVALEXPR =
    functor (Evalexpr : VAL) ->
        EVALEXPR with type t := Evalexpr.t

module MakeEvalExpr : MAKEEVALEXPR =
    functor (Evalexpr : VAL) ->
        struct
            type t = Evalexpr.t
            type expr =
                | Value of t
                | Add of expr * expr
                | Mul of expr * expr

            let add x y = Evalexpr.add x y
            let mul x y = Evalexpr.mul x y

            let rec eval = function
                | Value v          -> v
                | Add (a, b)       -> Evalexpr.add (eval a) (eval b)
                | Mul (a, b)       -> Evalexpr.mul (eval a) (eval b)
        end

module IntVal : (VAL with type t = int) = struct
    type t = int

    let add = ( + )
    let mul = ( * )
end

module FloatVal : (VAL with type t = float) = struct
    type t = float

    let add = ( +. )
    let mul = ( *. )
end

module StringVal : (VAL with type t = string) = struct
    type t = string

    let add s1 s2 = if (String.length s1) > (String.length s2) then
        s1 else s2
    let mul = ( ^ )
end

module IntEvalExpr : (EVALEXPR with type t := IntVal.t) = MakeEvalExpr (IntVal)
module FloatEvalExpr : (EVALEXPR with type t := FloatVal.t) = MakeEvalExpr (FloatVal)
module StringEvalExpr : (EVALEXPR with type t := StringVal.t) = MakeEvalExpr (StringVal)

let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2)
let fe = FloatEvalExpr.Add (FloatEvalExpr.Value 41.5, FloatEvalExpr.Value 0.92)
let se = StringEvalExpr.Mul (StringEvalExpr.Value "very ",
                        (StringEvalExpr.Add (StringEvalExpr.Value "very long",
                        StringEvalExpr.Value "short")))

let () = Printf.printf "Res = %d\n" (IntEvalExpr.eval ie)
let () = Printf.printf "Res = %f\n" (FloatEvalExpr.eval fe)
let () = Printf.printf "Res = %s\n" (StringEvalExpr.eval se)
