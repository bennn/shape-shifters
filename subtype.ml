open Definitions

let sDEBUG = false

(* [condition_ok ctx cond] Check if the 'satisfies' condition [cond] holds
   in context [ctx]. *)
let rec condition_ok (ctx:Context.t) (cond:cond_t) : bool =
  let () = if sDEBUG then Format.printf "[condition_ok] checking %s\n" (string_of_cond_t cond) in
  begin match cond with
  | Sat (v, shape)
  | SuperSat (v, shape) -> satisfies ctx (Context.find_tau_o ctx v) shape
  | NoCond -> true
  end
(* [satisfies ctx tt shp] Check if type [tt] is declared to satisfy shape [shp]
   in the current context [ctx]. *)
and satisfies (ctx:Context.t) (tt:type_t) (shp:shape_t) : bool =
  let () = if sDEBUG then Format.printf "[satisfies] %s <~ %s\n" (string_of_type_t tt) (string_of_shape_t shp) in
  begin match tt with
  | Bot -> true
  | Top -> false
  | TVar var
  | Super var -> satisfies ctx (Context.find_tau_o ctx var) shp
  | Instance (name, tc') ->
     let shifted = List.map (fun x -> (NoCond, x)) (Context.find_shifted ctx name) in
     let shapes =
       begin match Context.find_sig ctx name with
       | C (Class (_,_,_,_,shps,_)) -> shps
       | I (Interface (_,_,_,shps,_)) -> shps
       end
     in (* 2014-11-11: Leaving context, but possibly want to remove shifters *)
     shape_satisfies ctx (shifted @ shapes) shp
  end
(* [shape_satisfies ctx shapes shp] Depth-first search the inheritance hierarchy of
   the list of shapes [shapes] to find any one that satisfies [shp] in context [ctx]. *)
and shape_satisfies (ctx:Context.t) (shapes:(cond_t * shape_t) list) (shp:shape_t) : bool =
  begin match shapes with
  | [] -> false
  | (cond, shp')::tl ->
     let Shape (shp_name', shapes',_) = shp' in
     (* If condition fails, recurse on tail *)
     (* Else check for a match. Failing that search parents. Failing both, recurse. *)
     if not (condition_ok ctx cond)
     then shape_satisfies ctx tl shp
     else ((name_of_shape_t shp) = shp_name')
          || (shape_satisfies ctx shapes' shp || shape_satisfies ctx tl shp)
  end

(* [filter_by_condition ctx exprs] Check the list of tuples [exprs].
   Return the second element of every pair whose condition (first element)
   passes in the context [ctx]. *)
let rec filter_by_condition (ctx:Context.t) (guarded_exprs:(cond_t * 'a) list) : 'a list =
  begin match guarded_exprs with
  | [] -> []
  | (cond,x)::tl when condition_ok ctx cond ->
     let () = if sDEBUG then Format.printf "[filter_by_condition] condition '%s' passed\n" (string_of_cond_t cond) in
     x :: filter_by_condition ctx tl
  | _::tl ->
     let () = if sDEBUG then Format.printf "[filter_by_condition] condition '%s' failed\n" (string_of_cond_t (fst (List.hd guarded_exprs))) in
     filter_by_condition ctx tl
  end

(* [inherits ctx t1 t2] Search valid parents of [t1] for the name of [t2].
   Validity determined by the context [t1]. *)
let rec inherits (ctx:Context.t) (t1:type_t) (t2:type_t) : sig_t option =
  let () = if sDEBUG then Format.printf "[inherits] '%s' <:: '%s'\n" (string_of_type_t t1) (string_of_type_t t2) in
  begin match t1, t2 with
  | Instance (name1, _) , Instance (name2, _) when name1 = name2 -> None
  | Instance (name1, tc1) , Instance (name2, _)        ->
     let parent_sigs =
       begin match Context.find_sig ctx name1 with
       | C (Class (_, _, exts, iexts, _, _))  ->
          let ext_parents = List.map (fun (p,c) -> (p, C c)) exts
          and inh_parents = List.map (fun (p,i) -> (p, I i)) iexts in
          ext_parents @ inh_parents
       | I (Interface (_, _, iexts, _, _)) ->
          List.map (fun (p,i) -> (p, I i)) iexts
       end
     in
     let ctx' = Context.add_vars ctx tc1 in
     let filtered_parents = filter_by_condition ctx' parent_sigs in
     (* Helper function for the fold *)
     let search_parents acc parent =
       let p_name = name_of_sig_t parent in
       begin match acc with
       | Some _ -> acc (* short-circuit *)
       | None when p_name = name2 -> Some parent
       | None   -> inherits ctx (Instance(p_name, tc1)) t2
       end
     in
     List.fold_left search_parents None filtered_parents
  | _ -> None
  end

(* [inherits_eq ctx t1 t2] Follow the inheritance hierarchy, see if
   any (valid in [ctx]) parent extends the type [t2]. Return the [sig_t] if so,
   else return [None].
   The function [subtype] depends on this: need the name and param name
   of the parent. *)
let inherits_eq (ctx:Context.t) (t1:type_t) (t2:type_t) : sig_t option =
  let () = if sDEBUG then Format.printf "[inherits_eq] '%s' <::= '%s'\n" (string_of_type_t t1) (string_of_type_t t2) in
  begin match t1, t2 with
  | Instance (name1, _), Instance (name2, _) when name1 = name2 -> Some (Context.find_sig ctx name1)
  | Instance _, Instance _ -> inherits ctx t1 t2
  | _ -> None (* because this function is guarded by subtype *)
  end

let rec subtype (ctx:Context.t) (t1:type_t) (t2:type_t) : bool =
  let () = if sDEBUG then Format.printf "[subtype] '%s' <: '%s'\n" (string_of_type_t t1) (string_of_type_t t2) in
  begin match t1, t2 with
  (* Resolve type variables *)
  | TVar v, _  -> let t1' = Context.find_tau_o ctx v in
                  subtype ctx t1' t2
  | _, TVar v  -> let t2' = Context.find_tau_o ctx v in
                  subtype ctx t1  t2'
  (* supers are a little scary, take the lower bound. FOR NOW. *)
  | Super v, _ -> let t1' = Context.find_tau_o ctx v in
                  subtype ctx t1' t2
  | _, Super v -> let t2' = Context.find_tau_o ctx v in
                  subtype ctx t1  t2'
  (* Easy cases: bot/top *)
  | Bot, _ -> true
  | _, Top -> true
  | Top, _ -> false
  | _, Bot -> false
  (* the real deal *)
  | Instance (name1, tc1), Instance (name2, tc2) ->
     begin match inherits_eq ctx t1 t2 with
     | None -> let () = if sDEBUG then Format.printf "[subtype] <:: failed\n" in
               false
     | Some inhr_sig ->
        let tvars = params_of_sig_t inhr_sig in
        let ctx1 = Context.add_vars ctx tc1 in (* TODO scared *)
        let ctx2 = Context.add_vars ctx tc2 in
        List.fold_left
          (fun all_pass var -> all_pass && ( (* TODO scared about t_i t_o *)
                                 let t_i   = Context.find_tau_i ctx1 var in
                                 let t_o   = Context.find_tau_o ctx1 var in
                                 let t_i'  = Context.find_tau_i ctx2 var in
                                 let t_o'  = Context.find_tau_o ctx2 var in
                                 subtype ctx t_i' t_i && subtype ctx t_o t_o'))
          true tvars
     end
  end

(* [subtype_method ctx m1 m2] True if method [m1] is a subtype of method [m2]
   in context [ctx]. *)
let rec subtype_method (ctx:Context.t) (m1:method_t) (m2:method_t) : bool =
  let () = if sDEBUG then Format.printf "[subtype_method] '%s' <: '%s'\n" (string_of_method_t m1) (string_of_method_t m2) in
  let Method (ret1, _, args1) = m1 in
  let Method (ret2, _, args2) = m2 in
  let ctx' = Context.flip_variance ctx in
  subtype ctx ret1 ret2
  && (if sDEBUG then Format.printf "[subtype_method] return types ok!\n"; true)
  && for_all2 (fun (Arg(_,n1)) (Arg(_,n2)) -> n1 = n2) args1 args2
  && (if sDEBUG then Format.printf "[subtype_method] arg lengths/names ok!\n"; true)
  && for_all2 (fun (Arg(t1,_)) (Arg(t2,_)) -> subtype ctx' t2 t1) args1 args2
  && (if sDEBUG then Format.printf "[subtype_method] arg subtypes ok!\n"; true)
