open Definitions
open Subtype
open Well_formed

let rec join ((ctxA, sigA):Context.t * sig_t) ((ctxB, sigB):Context.t * sig_t) : sig_t =
  failwith "NOPE"

let rec satisfying_parent (ctx:Context.t) (st:sig_t) (method_name:string) : method_t option =
  failwith ""

(* [call_extension (c1,s1) (c2,s2) mname] Determine the type of the result of
   calling [s1:mname(s2)]. Undefined if [mname] is not declared by a shape. *)
let rec call_extension ((ctxA, sigA):Context.t * sig_t) ((ctxB, sigB):Context.t * sig_t)
                       (method_name:string) : string option =
  let joinAB = join (ctxA, sigA) (ctxB, sigB) in
  let ctx' = Context.merge ctxA ctxB in
  begin match find_satisfying_parent ctx' joinAB method_name with
  | None -> None
  | Some parent ->
     begin match lookup_method ctx' parent method_name with
     | None -> None
     | Some (Method(rt,_,_)) -> Some (Pretty_print.string_of_type_t_shallow ctx' rt)
     end
  end
