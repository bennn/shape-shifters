open Definitions

type t = Definitions.type_context

(* [add_var tc k v] Map the variable [k] to the value [v] in type context [tc].
   Overwrite old binding. *)
let add_var (tc:t) (k:string) ((v1,v2):type_t*type_t) : t =
  (k,v1,v2) :: tc

(* [find_opt tc k] Lookup the value associated with [k] in context [tc]. *)
let rec find_opt (tc:t) (k:string) : (type_t * type_t) option =
  begin match tc with
  | [] -> None
  | (k',v1,v2)::_ when k = k' -> Some (v1, v2)
  | _::rest -> find_opt rest k
  end

(* [find_tau_i tc k] Lookup the coNTRAvariant part of the variable [k] in context [tc]. *)
let find_tau_i (tc:t) (k:string) : type_t =
  begin match find_opt tc k with
  | Some (tau_i,_) -> tau_i
  | None           -> failwith (Format.sprintf "Unbound variable '%s'" k)
  end

(* [find_tau_o tc k] Lookup the covariant part of the variable [k] in context [tc]. *)
let find_tau_o (tc:t) (k:string) : type_t =
  begin match find_opt tc k with
  | Some (_,tau_o) -> tau_o
  | None           -> failwith (Format.sprintf "Unbound variable '%s'" k)
  end

let of_list (xs:Definitions.type_context) : t =
  xs

let print_triple ((k,v1,v2):(string*type_t*type_t)) : string =
  Format.sprintf ".%s -> (%s,%s)." k (string_of_type_t v1) (string_of_type_t v2)

let to_string (tc:t) : string =
  string_of_list print_triple tc
