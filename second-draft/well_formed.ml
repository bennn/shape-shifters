open Definitions
open Subtype

module MethodSet = Set.Make (struct
  type t = method_t
  let compare m1 m2 =
    let Method (_,n1,_) = m1 in
    let Method (_,n2,_) = m2 in
    Pervasives.compare n1 n2
end)

let rec filter_by_condition (ctx:context) (guarded_exprs:(cond_t * 'a) list) : 'a list =
  begin match guarded_exprs with
  | [] -> []
  | (cond,x)::tl when condition_ok cond ctx -> x :: filter_by_condition ctx tl
  | _::tl -> filter_by_condition ctx tl
  end

let union_fold f xs =
  List.fold_left (fun acc x -> MethodSet.union acc (f x)) MethodSet.empty xs

(* [inherited_methods ctx cs is ss] Collect the set of all methods from the classes [cs],
   the interfaces [it], and the shapes [ss]. *)
let rec inherited_methods (ctx:context)
                          (classes:((cond_t * class_t)  list))
                          (inters:((cond_t * inter_t)  list))
                          (shapes:((cond_t * shape_t)  list)) : MethodSet.t =
  union_fold (fun x -> x)
    [(union_fold (methods_of_class ctx) (filter_by_condition ctx classes))
    ; (union_fold (methods_of_inter ctx) (filter_by_condition ctx inters))
    ; (union_fold (methods_of_shape ctx) (filter_by_condition ctx shapes))]
and methods_of_class (ctx:context) (ct:class_t) : MethodSet.t =
  let Class(_,_, extends, impls, sats, mthds) = ct in
  let m_set = MethodSet.of_list (List.map fst (filter_by_condition ctx mthds)) in
  MethodSet.union m_set (inherited_methods ctx extends impls sats)
and methods_of_inter (ctx:context) (it:inter_t) : MethodSet.t =
  let Interface(_,_, impls, sats, mthds) = it in
  let m_set = MethodSet.of_list (filter_by_condition ctx mthds) in
  MethodSet.union m_set (inherited_methods ctx [] impls sats)
and methods_of_shape (ctx:context) (st:shape_t) : MethodSet.t =
  let Shape(_, sats, mthds) = st in
  let m_set = MethodSet.of_list (filter_by_condition ctx mthds) in
  MethodSet.union m_set (inherited_methods ctx [] [] sats)

(* param ok, class ok, interface ok, ...... *)
let rec class_ok (ctx:context) (ct:class_t) : bool =
  let Class(_, params, extends, impls, sats, mthds) = ct in
  let method_sigs = List.map (fun (c,(m,b)) -> (c,m)) mthds in
  (List.for_all (param_ok ctx) params)
  && (List.for_all (class_ok ctx)       (filter_by_condition ctx extends))
  && (List.for_all (interface_ok ctx)   (filter_by_condition ctx impls))
  && (List.for_all (shape_ok ctx)       (filter_by_condition ctx sats))
  && (List.for_all (method_body_ok ctx) (filter_by_condition ctx mthds))
  && (method_sigs_ok ctx (inherited_methods ctx extends impls sats) method_sigs)
  && (methods_implemented ctx (inherited_methods ctx [] impls sats) method_sigs)
(* [param_ok ctx param] Check that the parameter [param]
   is bound in the current context [ctx]. *)
and param_ok (ctx:context) (param:string) : bool =
  let _, _, var_c = ctx in
  let is_bound = fun v -> try ignore (var_c v); true with _ -> false in
  is_bound param
(* [interface_ok ctx it] Check that the interface [it] is valid in context [ctx]. *)
and interface_ok (ctx:context) (it:inter_t) : bool =
  let Interface(_, params, impls, sats, mthds) = it in
  (List.for_all (param_ok ctx) params)
  && (List.for_all (interface_ok ctx) (filter_by_condition ctx impls))
  && (List.for_all (shape_ok ctx)     (filter_by_condition ctx sats))
  && (method_sigs_ok ctx (inherited_methods ctx [] impls sats) mthds)
(* [shape_ok ctx st] Check that a shape [st] is valid in context [ctx]. *)
and shape_ok (ctx:context) (st:shape_t) : bool =
  let Shape(_, sats, mthds) = st in
  (List.for_all (shape_ok ctx) (filter_by_condition ctx sats))
  && method_sigs_ok ctx (inherited_methods ctx [] [] sats) mthds
(* [method_sigs_ok ctx inhr mthds] Check the list of methods [mthds].
   The list [mthds] should have no duplicates, and each [mthd] valid in context [ctx]
   must have a valid type in context [ctx] and should comply with the most-recent
   matching inherited method from [inhr]. *)
and method_sigs_ok (ctx:context) (inherited:MethodSet.t) (mthds:((cond_t * method_t) list)) : bool =
  let no_duplicates = (=) (MethodSet.cardinal (MethodSet.of_list (List.map snd mthds)))
                          (List.length mthds)
  in
  let method_type_ok (m:method_t) =
    let Method(ret,_,args) = m in
    (type_ok ctx ret)
    && (List.for_all (fun (Arg(tp,_)) -> type_ok ~sel:fst ctx tp) args)
  in
  let method_compliant (m:method_t) =
    if MethodSet.mem m inherited
    then subtype_method ctx m (MethodSet.find m inherited)
    else true
  in
  no_duplicates
  (* condition fails or method well-formed *)
  && List.for_all (fun m -> (method_type_ok m && method_compliant m))
                  (filter_by_condition ctx mthds)
(* [methods_implemented ctx sats mthds] Check that each method in [sats] is matched
   by an implementation in [mthds]. *)
and methods_implemented (ctx:context) (sats:MethodSet.t) (mthds:(cond_t * method_t) list) : bool =
  let diff = MethodSet.diff sats
                            (MethodSet.of_list (filter_by_condition ctx mthds)) in
  MethodSet.is_empty diff
(* [method_body_ok ctx (m,b)] Type-check the statement [b], make sure it
   conforms to the method signature [m]. *)
and method_body_ok (ctx:context) ((mthd,body):method_t * stmt_t) : bool =
  begin match body with
  | Null -> true
  end
(* [type_ok ?sel ctx tt] Check that the type [tt] is valid in context [ctx].
   I think this is just 'are all variables bound?' but idk... TODO *)
and type_ok ?(sel=snd) (ctx:context) (tt:type_t) : bool =
  let _ = sel in (* TODO !!! use the selector, flip variance! *)
  begin match tt with
  | Top | Bot -> true
  | TVar v | Super v -> param_ok ctx v
  | Instance(name, _) -> true (* TODO keep the var map? yo... idk *)
  end
and sig_ok (ctx:context) (st:sig_t) : bool =
  begin match st with
  | C ct -> class_ok ctx ct
  | I it -> interface_ok ctx it
  end