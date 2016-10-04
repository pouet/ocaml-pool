let rec hfs_f n =
    if n < 0 then -1
    else if n == 0 then 1
    else begin
        let tmp = hfs_f (n - 1) in
        n - hfs_m tmp
    end
and hfs_m n =
    if n < 0 then -1
    else if n == 0 then 0
    else begin
        let tmp = hfs_m (n - 1) in
        n - hfs_f tmp
    end

let test f n =
    let tmp = f n in
    print_int tmp;
    print_endline ""

let () =
    test hfs_m 0;
    test hfs_f 0;
    test hfs_m 4;
    test hfs_f 4
