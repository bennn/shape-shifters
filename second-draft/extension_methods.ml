open Definitions
open Subtype
open Well_formed

(* very naive *)
let rec join ((ctxA, sigA):Context.t * sig_t) ((ctxB, sigB):Context.t * sig_t) : sig_t =
  failwith "noooo"

(* [satisfying_parent ctx sts mname] Search the queue [sts] for the first sig
   declared to satisfy a shape that provides the method name [mname]. *)
let rec satisfying_parent (ctx:Context.t) (sts:sig_t list) (method_name:string) : sig_t option =
  begin match sts with
  | [] -> None
  | st::tl ->
     let cname = name_of_sig_t st in
     let shapes  = (Context.find_shifted ctx cname)
                   @ (filter_by_condition ctx (shapes_of_sig_t st)) in
     let s_methods = union_fold (methods_of_shape ctx) shapes in
     let key = Method(Bot, method_name, []) in
     begin match MethodSet.mem key s_methods with
     | true  -> Some st
     | false -> let supers = filter_by_condition ctx (parent_sigs_of_sig_t st) in
                satisfying_parent ctx (tl @ supers) method_name
     end
  end

(* [call_extension (c1,s1) (c2,s2) mname] Determine the type of the result of
   calling [s1:mname(s2)]. Undefined if [mname] is not declared by a shape. *)
let rec call_extension ((ctxA, sigA):Context.t * sig_t) ((ctxB, sigB):Context.t * sig_t)
                       (method_name:string) : string option =
  let joinAB = join (ctxA, sigA) (ctxB, sigB) in
  let ctx' = Context.merge ctxA ctxB in
  begin match satisfying_parent ctx' [joinAB] method_name with
  | None -> None
  | Some parent ->
     begin match lookup_method ctx' parent method_name with
     | None -> None
     | Some (Method(rt,_,_)) -> Some (Pretty_print.string_of_type_t_shallow ctx' rt)
     end
  end
