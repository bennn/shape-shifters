open Definitions
open Subtype
open Well_formed

let eDEBUG = false

let rec join_search (f:type_t -> sig_t option) (ctx:Context.t) (to_search:sig_t list) : sig_t =
  let () = if eDEBUG then Format.printf "[join_search] ...\n" in
  begin match to_search with
  | [] -> failwith "NO JOIN"
  | st::tl -> let inst = instance_of_sig_t st in
              begin match f inst with
              | Some st -> st
              | None -> let parents = filter_by_condition ctx (parent_sigs_of_sig_t st) in
                        join_search f ctx (tl @ parents)
              end
  end

(* Very naive and wrong: if one inherits the other, return immediately. Otherwise do an
   asymmetrical BFS of one sig's hierarchy *)
let join ((ctxA, sigA):Context.t * sig_t) ((ctxB, sigB):Context.t * sig_t) : sig_t =
  let () = if eDEBUG then Format.printf "[join] %s |_| %s\n" (name_of_sig_t sigA) (name_of_sig_t sigB) in
  let inst1 = instance_of_sig_t sigA in
  let inst2 = instance_of_sig_t sigB in
  begin match inherits_eq ctxA inst1 inst2 with
  | Some st -> st
  | None ->
     begin match inherits_eq ctxB inst2 inst1 with
     | Some st -> st
     | None -> join_search (fun tt -> inherits_eq ctxA inst1 tt)
                           ctxB
                           (filter_by_condition ctxB (parent_sigs_of_sig_t sigB))
     end
  end

let rec join_type ctx t1 t2 =
  let () = if eDEBUG then Format.printf "[join_type] %s |t| %s\n" (string_of_type_t t1) (string_of_type_t t2) in
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

let rec set_parent_vars (ctx0,params) (ctx1,p1) (ctx2,p2) =
  begin match params, p1, p2 with
  | h0::rest0 , h1::rest1 , h2::rest2 ->
     let tau1 = Context.find_tau_o ctx1 h1 in
     let tau2 = Context.find_tau_o ctx2 h2 in
     let () = if eDEBUG then Format.printf "[set_parent_vars] instantiating %s from %s (= %s) and %s (= %s)\n" h0 h1 (string_of_type_t tau1) h2 (string_of_type_t tau2) in
     let joined = join_type ctx0 tau1 tau2 in
     let ctx0 = Context.add_covariant_var ctx0 h0 joined in
     set_parent_vars (ctx0,rest0) (ctx1,rest1) (ctx2,rest2)
  | _ -> ctx0
  end

(* [satisfying_parent ctx sts mname] Search the queue [sts] for the first sig
   declared to satisfy a shape that provides the method name [mname]. *)
let rec satisfying_parent (ctx:Context.t) (sts:sig_t list) (method_name:string) : sig_t option =
  let () = if eDEBUG then Format.printf "[satisfying_parent] searching for method '%s'\n" method_name in
  begin match sts with
  | [] -> None
  | st::tl ->
     begin match lookup_method ctx st method_name with
     | Some _ -> Some st
     | None -> let supers = filter_by_condition ctx (parent_sigs_of_sig_t st) in
               satisfying_parent ctx (tl @ supers) method_name
     end
  end

(* [call_extension (c1,s1) (c2,s2) mname] Determine the type of the result of
   calling [s1:mname(s2)]. Undefined if [mname] is not declared by a shape. *)
let rec call_extension ((ctxA, sigA):Context.t * sig_t) ((ctxB, sigB):Context.t * sig_t)
                       (method_name:string) : type_t option =
  let joinAB = join (ctxA, sigA) (ctxB, sigB) in
  let ctx' =
    set_parent_vars (Context.merge ctxA ctxB, params_of_sig_t joinAB)
                    (ctxA, params_of_sig_t sigA)
                    (ctxB, params_of_sig_t sigB)
  in
  begin match satisfying_parent ctx' [joinAB] method_name with
  | None -> None
  | Some parent ->
     begin match lookup_method ctx' parent method_name with
     | None -> None
     | Some (Method(rt,_,_)) -> Some (Context.apply_subst ctx' rt)
     end
  end
