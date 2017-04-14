type hour = int

let zero : hour = 12

let add (h : hour) (n : hour) : hour = (h + (n mod zero + 2 * zero)) mod zero
let sub (h : hour) (n : hour) : hour = ((n mod zero + 2 * zero) - h) mod zero
