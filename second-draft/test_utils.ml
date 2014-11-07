open Definitions

let empty_c = fun x ->
  if x = "THIS"
  then (Bot, Bot)
  else failwith (Format.sprintf "unbound variable '%s'" x)

let print_hdr s = Format.printf "\n##### %s\n" s
let tick ()     = Format.printf ".\n"

let test b = assert b; tick ()

let merge_disjoint (map1:'a StringMap.t) (map2:'a StringMap.t) : 'a StringMap.t =
  let f k v1 v2 =
    begin match v1 , v2 with
    | Some v , None
    | None   , Some v -> Some v
    | None   , None   -> None
    | Some _ , Some _ -> failwith "merge disjoint -- not disjoint!"
    end
  in
  StringMap.merge f map1 map2
