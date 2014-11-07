let empty_c = fun x ->
  if x = "THIS"
  then (Definitions.Bot, Definitions.Bot)
  else failwith (Format.sprintf "unbound variable '%s'" x)

let print_hdr s = Format.printf "\n##### %s\n" s
let tick ()     = Format.printf ".\n"

let test b = assert b; tick ()
