open Definitions

(** There is definitely room for improvement. #functors *)

type t = shape_t list

let empty = []

(* [of_list xs] Initialize a shape context from the list of shapes [xs]. *)
let of_list (xs:shape_t list) =
  (* TODO raise exception if any duplicates *)
  xs

(* [find_opt sc name] Search for the name [name] in shape context [sc]. *)
let rec find_opt (sc:t) (name:string) : shape_t option =
  begin match sc with
  | [] -> None
  | shp::_ when name = name_of_shape_t shp -> Some shp
  | _::tl -> find_opt tl name
  end

(* [find sc name] Lookup the name [name] in shape context [sc].
   Raise an exception if not found. *)
let find (sc:t) (name:string) : shape_t =
  begin match find_opt sc name with
  | Some shp -> shp
  | None     -> failwith (Format.sprintf "Shape '%s' unbound." name)
  end

let to_string (sc:t) : string =
  string_of_list name_of_shape_t sc
