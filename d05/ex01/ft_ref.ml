type 'a ft_ref = { mutable contents : 'a }

let return x = { contents = x }
let get x = x.contents
let set x y = x.contents <- y
let bind x (f : 'a -> 'b ft_ref) = f (get x)

let _ =
        let a = return 10 in
        Printf.printf "%d\n" (get a);
        set a 42;
        Printf.printf "%d\n" (get a);
        let b = bind a (fun x -> return (x / 2)) in
        Printf.printf "%d\n" (get a);
        Printf.printf "%d\n" (get b)
