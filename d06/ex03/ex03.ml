module type FIXED = sig
    type t

    val of_float : float -> t
    val of_int : int -> t
    val to_float : t -> float
    val to_int : t -> int
    val to_string : t -> string
    val zero : t
    val one : t
    val succ : t -> t
    val pred : t -> t
    val min : t -> t -> t
    val max : t -> t -> t
    val gth : t -> t -> bool
    val lth : t -> t -> bool
    val gte : t -> t -> bool
    val lte : t -> t -> bool
    val eqp : t -> t -> bool (** physical equality *)
    val eqs : t -> t -> bool (** structural equality *)
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val foreach : t -> t -> (t -> unit) -> unit
end

module type FIXEDBITS = sig
    val bits : int
end

module type MAKEFIXED =
    functor (Fixedbits : FIXEDBITS) -> FIXED

module Make : MAKEFIXED =
    functor (Fixedbits : FIXEDBITS) ->
        struct
            type t = int

            let fbits = Fixedbits.bits

            let of_float x = int_of_float (x *. (float_of_int (1 lsl fbits)))
            let of_int x = x lsl fbits

            let to_float x = (float_of_int x) /. (float_of_int (1 lsl fbits))
            let to_int x = x lsr fbits
            let to_string x = string_of_float (to_float x)

            let zero = 0
            let one = 1 lsl fbits

            let succ x = x + 1
            let pred x = x - 1

            let min x y = if x <= y then x else y
            let max x y = if x >= y then x else y

            let gth x y = x > y
            let lth x y = x < y
            let gte x y = x >= y
            let lte x y = x <= y

            let eqp x y = x == y
            let eqs x y = x = y

            let add x y = x + y
            let sub x y = x - y
            let mul x y = of_float ((to_float x) *. (to_float y))
            let div x y =
                if y = 0 then 0
                else of_float ((to_float x) /. (to_float y))

            let foreach x y f =
                let rec loopup i =
                    if i <= y then begin
                        f i;
                        loopup (i + 1)
                    end
                in
                let rec loopdown i =
                    if i <= y then begin
                        f i;
                        loopup (i - 1)
                    end
                in
                if x < y then loopup x
                else loopdown x
        end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
    let x8 = Fixed8.of_float 21.10 in
    let y8 = Fixed8.of_float 21.32 in
    let r8 = Fixed8.add x8 y8 in
    print_endline (Fixed8.to_string r8);
    Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
    
    let pf = Printf.printf in

    pf "of_float  : %f\n" (Fixed8.to_float x8);
    pf "of_int    : %d\n" (Fixed8.to_int x8);

    pf "zero      : %s\n" (Fixed8.to_string Fixed8.zero);
    pf "one       : %s\n" (Fixed8.to_string Fixed8.one);

    pf "pred      : %s\n" (Fixed8.to_string (Fixed8.pred Fixed8.one));
    pf "succ      : %s\n" (Fixed8.to_string (Fixed8.succ Fixed8.one));

    pf "min       : %s\n" (Fixed8.to_string (Fixed8.min Fixed8.zero Fixed8.one));
    pf "max       : %s\n" (Fixed8.to_string (Fixed8.max Fixed8.zero Fixed8.one));

    pf "gth       : %s\n" (if Fixed8.gth Fixed8.zero Fixed8.one then "true" else "false");
    pf "lth       : %s\n" (if Fixed8.lth Fixed8.zero Fixed8.one then "true" else "false");
    pf "gte       : %s\n" (if Fixed8.gte Fixed8.zero Fixed8.one then "true" else "false");
    pf "lte       : %s\n" (if Fixed8.lte Fixed8.zero Fixed8.one then "true" else "false");

    pf "eqp       : %s\n" (if Fixed8.eqp Fixed8.zero Fixed8.zero then "true" else "false");
    pf "eqs       : %s\n" (if Fixed8.eqs Fixed8.zero Fixed8.zero then "true" else "false");

    pf "add       : %s\n" (Fixed8.to_string (Fixed8.add x8 x8));
    pf "sub       : %s\n" (Fixed8.to_string (Fixed8.sub x8 x8));
    pf "mul       : %s\n" (Fixed8.to_string (Fixed8.mul x8 x8));
    pf "div       : %s\n" (Fixed8.to_string (Fixed8.div x8 x8));
