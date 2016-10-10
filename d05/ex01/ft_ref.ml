type 'a ft_ref = { mutable contents : 'a }

let return x = { contents = x }
let get x = x.contents
let set x y = x.contents <- y
let bind x f = f x
let bind x (f : 'a -> 'b ft_ref) = f x.contents
