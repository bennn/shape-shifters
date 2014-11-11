open Definitions

(** There is definitely room for improvement. #functors *)

(* One-to-many relation between instances (strings) and shape shifters *)
type t = (string * shifter_t) list

let empty = []

(* [of_list xs] Initialize a shifter context from the list of shifters [xs]. *)
let of_list (xs:(string * shifter_t) list) =
  (* TODO raise exception if any duplicates *)
  xs

(* [find sc name] Return a list of all shifters mapped from the name [name]. *)
let rec find (sc:t) (name:string) : shifter_t list =
  begin match sc with
  | [] -> []
  | (v,s)::tl when v = name -> s :: find tl name
  | _::tl -> find tl name
  end

let to_string (sc:t) : string =
  string_of_list (fun (v,s) -> Format.sprintf "(%s->%s)" v (name_of_shifter_t s)) sc
