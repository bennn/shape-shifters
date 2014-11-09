open Definitions

let tDEBUG = false

let empty_varmap = fun x ->
  if x = "THIS"
  then (Bot, Bot)
  else failwith (Format.sprintf "unbound variable '%s'" x)

let print_hdr s = Format.printf "\n## %s\n" s
let print_subhdr s = Format.printf "#### %s\n" s
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

let same_method_names (ctx:context) (t1:type_t) (t2:type_t) : bool =
  let n1 = Well_formed.method_names ctx t1 in
  let n2 = Well_formed.method_names ctx t2 in
  let () = if tDEBUG then Format.printf "\nN1 = [%s]\nN2 = [%s]\n" (String.concat "; " n1) (String.concat "; " n2) in
  begin match n1, n2 with
  | ["NULL"] , _
  | _ , ["NULL"]  -> true
  | [], [] -> true
  | [] , _
  | _ , [] -> false
  | xs, ys -> (=) (List.sort Pervasives.compare xs)
                  (List.sort Pervasives.compare ys)
  end
