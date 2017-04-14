type project = string * string * int

let zero : project = ("", "", 0)

let combine ((s, t, n) : project) ((s', t', n') : project) : project =
    let res = n + n in
    let str = if res > 80 then "succeed" else "failed" in
    (s ^ s', str, res)

let fail ((s, _, _) : project) : project =
    (s, "failed", 0)

let succeed ((s, _, _) : project) : project =
    (s, "succeed", 80)
