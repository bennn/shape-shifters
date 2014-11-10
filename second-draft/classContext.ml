open Definitions

type t = sig_t list

let empty = []

(* [of_list xs] Initialize a class context from the list of signatures [xs]. *)
let of_list (xs:sig_t list) =
  (* TODO raise exception if any duplicates *)
  xs

(* [find_opt cc name] Search for the name [name] in class context [cc]. *)
let rec find_opt (cc:t) (name:string) : sig_t option =
  begin match cc with
  | [] -> None
  | st::_ when name = name_of_sig_t st -> Some st
  | _::tl -> find_opt tl name
  end

(* [find cc name] Lookup the name [name] in class context [cc].
   Raise an exception if not found. *)
let find (cc:t) (name:string) : sig_t =
  begin match find_opt cc name with
  | Some st -> st
  | None    -> failwith (Format.sprintf "Class/Interface '%s' unbound." name)
  end

let to_string (cc:t) : string =
  string_of_list name_of_sig_t cc
