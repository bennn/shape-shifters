let empty_c = fun x -> failwith (Format.sprintf "unbound variable '%s'" x)

let print_hdr s = Format.printf "##### %s" s
