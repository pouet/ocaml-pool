type 'a t =
        | Success of 'a
        | Failure of exn

exception Fail

let return s = Success s

let bind el f =
        match el with
        | Success _     -> begin
                try f el
                with
                | e     -> Failure e
        end
        | _             -> el

let recover el f =
        match el with
        | Failure a     -> f a
        | _             -> el

let filter el f =
        match el with
        | Success a when f a = false -> Failure Fail
        | _                          -> el

let flatten el =
        match el with
        | Success a     -> begin
                match a with
                | Success _     -> a
                | Failure b     -> Failure b
        end
        | Failure a     -> Failure a

let to_string = function
        | Success s     -> "Success : " ^ s
        | Failure _     -> "Failure"
