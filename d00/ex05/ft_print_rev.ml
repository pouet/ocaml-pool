let ft_print_rev s =
    let rec print i =
        if i < 0 then print_char '\n'
        else begin
            print_char s.[i];
            print (i - 1)
        end
    in
    print (String.length s - 1)

let () =
    ft_print_rev "";
    ft_print_rev "24 uocuoc"
