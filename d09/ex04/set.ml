type 'a t = 'a list

let return a : 'a t = [ a ]

let bind (a : 'a t) (f : 'a -> 'b t) : 'b t =
    let rec aux = function
        | []        -> []
        | hd :: tl  -> f hd @ aux tl
    in
    aux a

let union (a : 'a t) (b : 'a t) : 'a t =
    a @ b

let inter (a : 'a t) (b : 'a t) : 'a t =
    let rec is_in el = function
        | []        -> false
        | hd :: tl  -> if hd = el then true
                        else is_in el tl
    in
    let rec aux = function
        | []        -> []
        | hd :: tl  -> if is_in hd b then hd :: aux tl
                        else aux tl
    in
    aux a

let diff (a : 'a t) (b : 'a t) : 'a t =
    let rec is_in el = function
        | []        -> false
        | hd :: tl  -> if hd = el then true
                        else is_in el tl
    in
    let rec aux = function
        | []        -> []
        | hd :: tl  -> if is_in hd b = false then hd :: aux tl
                        else aux tl
    in
    aux a

let filter (a : 'a t) f : 'a t =
    let rec aux = function
        | []        -> []
        | hd :: tl  -> if f hd then hd :: aux tl
                        else aux tl
    in
    aux a

let foreach (a : 'a t) (f : 'a -> unit) =
    let rec aux = function
        | []        -> ()
        | hd :: tl  -> (f hd; aux tl)
    in
    aux a

let for_all (a : 'a t) f =
    let rec aux = function
        | []        -> true
        | hd :: tl  -> if f hd then aux tl
                        else false
    in
    aux a

let exists (a : 'a t) f =
    let rec aux = function
        | []        -> false
        | hd :: tl  -> if f hd then true
                        else aux tl
    in
    aux a

