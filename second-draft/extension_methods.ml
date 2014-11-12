open Definitions
open Subtype
open Well_formed

(* very naive and wrong *)
let rec join ((ctxA, sigA):Context.t * sig_t) ((ctxB, sigB):Context.t * sig_t) : sig_t =
  let n1 = name_of_sig_t sigA in
  let n2 = name_of_sig_t sigB in
  (* let params1 = params_of_sig_t sigA in *)
  (* let params2 = params_of_sig_t sigB in *)
  let inst1 = Instance(n1, []) in
  let inst2 = Instance(n2, []) in
  let sig' =
    begin match inherits_eq ctxA inst1 inst2 with
    | Some st -> st
    | None ->
       begin match inherits_eq ctxB inst2 inst1 with
       | Some st -> st
       | None -> failwith "NO JOIN"
       end
    end
  in
  sig'

let rec join_type ctx t1 t2 =
  begin match t1, t2 with
  | Bot, x | x, Bot -> x
  | Top, _ | _, Top -> Top
  | TVar v, _  -> let t1' = Context.find_tau_o ctx v in
                  join_type ctx t1' t2
  | _, TVar v  -> let t2' = Context.find_tau_o ctx v in
                  join_type ctx t1  t2'
  (* supers are a little scary, take the lower bound. FOR NOW. *)
  | Super v, _ -> let t1' = Context.find_tau_o ctx v in
                  join_type ctx t1' t2
  | _, Super v -> let t2' = Context.find_tau_o ctx v in
                  join_type ctx t1  t2'
  | Instance (n1, _), Instance(n2, _) ->
     let c1 = Context.find_sig ctx n1 in
     let c2 = Context.find_sig ctx n2 in
     Instance(name_of_sig_t (join (ctx, c1) (ctx, c2)), [])
  end

(* [satisfying_parent ctx sts mname] Search the queue [sts] for the first sig
   declared to satisfy a shape that provides the method name [mname]. *)
let rec satisfying_parent (ctx:Context.t) (sts:sig_t list) (method_name:string) : sig_t option =
  let () = Format.printf "LOOKING for asat parent for ehtomd '%s'\n" method_name in
  begin match sts with
  | [] -> None
  | st::tl ->
     begin match lookup_method ctx st method_name with
     | Some _ -> Some st
     | None -> let supers = filter_by_condition ctx (parent_sigs_of_sig_t st) in
               satisfying_parent ctx (tl @ supers) method_name
     end
  end

(* let rec set_parent_vars ctx params p1 p2 = *)
(*   begin match params, p1, p2 with *)
(*   | h1::t1 , h2::t2 , h3::t3 -> *)
(*      let ctx' = Context.add_covariant_var ctx h1 (join_type ctx *)
(*                                                             (Context.find_tau_o ctx h3) *)
(*                                                             (Context.find_tau_o ctx h2) *)
(*                                               ) in *)
(*      set_parent_vars ctx' t1 t2 t3 *)
(*   | _ -> ctx *)
(*   end *)

(* [call_extension (c1,s1) (c2,s2) mname] Determine the type of the result of
   calling [s1:mname(s2)]. Undefined if [mname] is not declared by a shape. *)
let rec call_extension ((ctxA, sigA):Context.t * sig_t) ((ctxB, sigB):Context.t * sig_t)
                       (method_name:string) : string option =
  let joinAB = join (ctxA, sigA) (ctxB, sigB) in
  let ctx' = ctxB
    (* set_parent_vars (Context.merge ctxA ctxB) (params_of_sig_t joinAB) *)
    (*                          (params_of_sig_t sigA) (params_of_sig_t sigB) *)
  in
  begin match satisfying_parent ctx' [joinAB] method_name with
  | None -> None
  | Some parent ->
     begin match lookup_method ctx' parent method_name with
     | None -> None
     | Some (Method(rt,_,_)) -> Some (Pretty_print.string_of_type_t_shallow ctx' rt)
     end
  end
