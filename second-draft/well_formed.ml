open Definitions
open Subtype

let wDEBUG = true

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
  | (cond,x)::tl when condition_ok cond ctx ->
     let () = if wDEBUG then Format.printf "[filter_by_condition] condition '%s' passed\n" (string_of_cond_t cond) in
     x :: filter_by_condition ctx tl
  | _::tl ->
     let () = if wDEBUG then Format.printf "[filter_by_condition] condition '%s' failed\n" (string_of_cond_t (fst (List.hd guarded_exprs))) in
     filter_by_condition ctx tl
  end

let union_fold f xs =
  List.fold_left (fun acc x -> MethodSet.union acc (f x)) MethodSet.empty xs

(* [inherited_methods ctx cs is ss] Collect the set of all methods from the classes [cs],
   the interfaces [it], and the shapes [ss]. *)
let rec inherited_methods (ctx:context)
                          (classes:((cond_t * class_t)  list))
                          (inters:((cond_t * inter_t)  list))
                          (shapes:((cond_t * shape_t)  list)) : MethodSet.t =
  let () = if wDEBUG then Format.printf "[inherited_methods]\n" in
  union_fold (fun x -> x)
    [(union_fold (methods_of_class ctx) (filter_by_condition ctx classes))
    ; (union_fold (methods_of_inter ctx) (filter_by_condition ctx inters))
    ; (union_fold (methods_of_shape ctx) (filter_by_condition ctx shapes))]
and methods_of_class (ctx:context) (ct:class_t) : MethodSet.t =
  let () = if wDEBUG then Format.printf "[methods_of_class] '%s'\n" (string_of_class_t ct) in
  let Class(_,_, extends, impls, sats, mthds) = ct in
  let m_set = MethodSet.of_list (List.map fst (filter_by_condition ctx mthds)) in
  MethodSet.union m_set (inherited_methods ctx extends impls sats)
and methods_of_inter (ctx:context) (it:inter_t) : MethodSet.t =
  let () = if wDEBUG then Format.printf "[methods_of_inter] '%s'\n" (string_of_inter_t it) in
  let Interface(_,_, impls, sats, mthds) = it in
  let m_set = MethodSet.of_list (filter_by_condition ctx mthds) in
  MethodSet.union m_set (inherited_methods ctx [] impls sats)
and methods_of_shape (ctx:context) (st:shape_t) : MethodSet.t =
  let () = if wDEBUG then Format.printf "[methods_of_shape] '%s'\n" (string_of_shape_t st) in
  let Shape(_, sats, mthds) = st in
  let m_set = MethodSet.of_list (filter_by_condition ctx mthds) in
  MethodSet.union m_set (inherited_methods ctx [] [] sats)

(* [lookup_method ctx ct mname] Search [ct] and its parents for the method with name [mname].
   First look in your own method set, then call for inherited methods. *)
let rec lookup_method (ctx:context) (st:sig_t) (mname:string) : method_t option =
  (* dammit, should be using a string map for methods *)
  let key = Method(Bot, mname, []) in
  begin match st with
  | C (Class(_,_,exts,_,_,mthds)) ->
     let m_set = MethodSet.of_list (List.map fst (filter_by_condition ctx mthds)) in
     begin match MethodSet.mem key m_set with
     | true  -> Some (MethodSet.find key m_set)
     | false -> let m_inhr = inherited_methods ctx exts [] [] in
                begin match MethodSet.mem key m_inhr with
                | true -> Some (MethodSet.find key m_inhr)
                | false -> None
                end
     end
  | I (Interface(_,_,impls,shps,mthds)) ->
     let m_set = MethodSet.of_list (filter_by_condition ctx mthds) in
     begin match MethodSet.mem key m_set with
     | true  -> Some (MethodSet.find key m_set)
     | false -> let m_inhr = inherited_methods ctx [] impls shps in
                begin match MethodSet.mem key m_inhr with
                | true  -> Some (MethodSet.find key m_inhr)
                | false -> None
                end
     end
  end

(* param ok, class ok, interface ok, ...... *)
let rec class_ok (ctx:context) (ct:class_t) : bool =
  let () = if wDEBUG then Format.printf "[class_ok] '%s'\n" (string_of_class_t ct) in
  let Class(name, params, extends, impls, sats, mthds) = ct in
  let ctx = (* set THIS *)
    let (_,_,(_,vm)) = ctx in
    context_addvar ctx name (Instance(name, vm), Instance(name, vm))
  in
  let method_sigs = List.map (fun (c,(m,b)) -> (c,m)) mthds in
  (List.for_all (param_ok ctx) params)
  && (if wDEBUG then Format.printf "[class_ok] class '%s' params ok\n" (string_of_class_t ct); true)
  && (List.for_all (class_ok ctx)       (filter_by_condition ctx extends))
  && (if wDEBUG then Format.printf "[class_ok] class '%s' classs ok\n" (string_of_class_t ct); true)
  && (List.for_all (interface_ok ctx)   (filter_by_condition ctx impls))
  && (if wDEBUG then Format.printf "[class_ok] class '%s' inters ok\n" (string_of_class_t ct); true)
  && (List.for_all (shape_ok ctx)       (filter_by_condition ctx sats))
  && (if wDEBUG then Format.printf "[class_ok] class '%s' shapes ok\n" (string_of_class_t ct); true)
  && (List.for_all (method_body_ok ctx) (filter_by_condition ctx mthds))
  && (if wDEBUG then Format.printf "[class_ok] class '%s' bodies ok\n" (string_of_class_t ct); true)
  && (method_sigs_ok ctx (inherited_methods ctx extends impls sats) method_sigs)
  && (if wDEBUG then Format.printf "[class_ok] class '%s' sigs ok\n" (string_of_class_t ct); true)
  && (let to_implement = MethodSet.diff (inherited_methods ctx [] impls sats)
                                        (inherited_methods ctx extends [] [])
      in methods_implemented ctx to_implement method_sigs)
  && (if wDEBUG then Format.printf "[class_ok] class '%s' approved\n" (string_of_class_t ct); true)
(* [param_ok ctx param] Check that the parameter [param]
   is bound in the current context [ctx]. *)
and param_ok (ctx:context) (param:string) : bool =
  let () = if wDEBUG then Format.printf "[param_ok] '%s'\n" param in
  let is_bound = fun v -> try ignore (lookup_tau_o ctx v); true with _ -> false in
  is_bound param
(* [interface_ok ctx it] Check that the interface [it] is valid in context [ctx]. *)
and interface_ok (ctx:context) (it:inter_t) : bool =
  let () = if wDEBUG then Format.printf "[interface_ok] '%s'\n" (string_of_inter_t it) in
  let Interface(name, params, impls, sats, mthds) = it in
  let ctx = (* set THIS *)
    let (_,_,(_,vm)) = ctx in
    let () = if wDEBUG then Format.printf "[interface_ok] BINDING THIS!!!!!!!!\n" in
    context_addvar ctx name (Instance(name, vm), Instance(name, vm))
  in
  (List.for_all (param_ok ctx) params)
  && (List.for_all (interface_ok ctx) (filter_by_condition ctx impls))
  && (List.for_all (shape_ok ctx)     (filter_by_condition ctx sats))
  && (method_sigs_ok ctx (inherited_methods ctx [] impls sats) mthds)
(* [shape_ok ctx st] Check that a shape [st] is valid in context [ctx]. *)
and shape_ok (ctx:context) (st:shape_t) : bool =
  let () = if wDEBUG then Format.printf "[shape_ok] '%s'\n" (string_of_shape_t st) in
  let Shape(name, sats, mthds) = st in
  let ctx = (* set THIS *)
    let (_,_,(_,vm)) = ctx in
    context_addvar ctx name (Instance(name, vm), Instance(name, vm))
  in
  (List.for_all (shape_ok ctx) (filter_by_condition ctx sats))
  && method_sigs_ok ctx (inherited_methods ctx [] [] sats) mthds
(* [method_sigs_ok ctx inhr mthds] Check the list of methods [mthds].
   The list [mthds] should have no duplicates, and each [mthd] valid in context [ctx]
   must have a valid type in context [ctx] and should comply with the most-recent
   matching inherited method from [inhr]. *)
and method_sigs_ok (ctx:context) (inherited:MethodSet.t) (mthds:((cond_t * method_t) list)) : bool =
  let () = if wDEBUG then Format.printf "[method_sigs_ok] \n" in
  let no_duplicates = (=) (MethodSet.cardinal (MethodSet.of_list (List.map snd mthds)))
                          (List.length mthds)
  in
  let () = if wDEBUG && (not no_duplicates) then Format.printf "[method_sigs_ok] NOT OKAY: duplicate method in '%s'\n" (String.concat "; " (List.sort Pervasives.compare (List.map string_of_method_t (List.map snd mthds)))) in
  let ctx' = flip_variance ctx in
  let method_type_ok (m:method_t) =
    let () = if wDEBUG then Format.printf "[method_sigs_ok.type_ok] '%s'\n" (string_of_method_t m) in
    let Method(ret,_,args) = m in
    (type_ok ctx ret)
    && (List.for_all (fun (Arg(tp,_)) -> type_ok ctx' tp) args)
  in
  let method_compliant (m:method_t) =
    let () = if wDEBUG then Format.printf "[method_sigs_ok.compliant] '%s'\n" (string_of_method_t m) in
    if MethodSet.mem m inherited
    then let () = if wDEBUG then Format.printf "[method_sigs_ok.compliant] is method '%s' a subtype of method '%s'???\n" (string_of_method_t m) (string_of_method_t (MethodSet.find m inherited)) in
      subtype_method ctx m (MethodSet.find m inherited)
    else true
  in
  no_duplicates
  (* condition fails or method well-formed *)
  && List.for_all (fun m -> (method_type_ok m && method_compliant m))
                  (filter_by_condition ctx mthds)
  && (if wDEBUG then Format.printf "[method_sigs_ok] ALL PASS\n"; true)
(* [methods_implemented ctx todo mthds] Check that each method in [todo] is matched
   by an implementation in [mthds]. *)
and methods_implemented (ctx:context) (to_implement:MethodSet.t) (mthds:(cond_t * method_t) list) : bool =
  let mthds' = filter_by_condition ctx mthds in
  let () = if wDEBUG then Format.printf "[methods_implemented] comparing '%s' with '%s'\n"
                                        (String.concat "; " (MethodSet.fold (fun x acc -> string_of_method_t x :: acc) to_implement []))
                                        (String.concat "; " (List.map string_of_method_t mthds')) in
  let diff = MethodSet.diff to_implement
                            (MethodSet.of_list mthds') in
  MethodSet.is_empty diff
  && (if wDEBUG then Format.printf "[methods_implemented] OKAY\n"; true)
(* [method_body_ok ctx (m,b)] Type-check the statement [b], make sure it
   conforms to the method signature [m]. As of (2014-11-09), just make sure
   the expression type matches the return type. *)
and method_body_ok (ctx:context) ((mthd,body):method_t * stmt_t) : bool =
  let () = if wDEBUG then Format.printf "[method_body_ok] '%s'\n" (string_of_method_t mthd) in
  let Method(expected_rtype, _, _) = mthd in
  begin match body with
  | Return Null -> true
  | Return (New (cname, vm)) ->
     let () = if wDEBUG then Format.printf "[method_body_ok.return_new] '%s'\n" cname in
     let ct = Instance(cname, vm) in
     (type_ok ctx ct)
     && (match lookup_class ctx cname with C _ -> true | I _ -> false)
     && (subtype ctx ct expected_rtype)
  | Return (Call ((cname, vm), mname, args)) ->
     let () = if wDEBUG then Format.printf "[method_body_ok.call] '%s.%s'\n" cname mname in
     let ctx' = context_addvarmap ctx vm in
     let cls = lookup_class ctx' cname in
     (type_ok ctx' (Instance(cname, vm)))
     && (match lookup_method ctx' cls mname with
         | None -> if wDEBUG then Format.printf "[method_body_ok] method '%s.%s' not found\n" cname mname; false
         | Some (Method(actual_rtype, _, args')) ->
            let arg_vals = List.map (fun (a,b) -> Instance(a,b)) args in
            let arg_sigs = List.map (fun (Arg(a,_)) -> a)        args' in
            let ctx'' = flip_variance ctx' in
            (for_all2 (subtype ctx'') arg_sigs arg_vals)
            && (subtype ctx' actual_rtype expected_rtype))
  | Return (ExtM (cname, mname, args)) -> failwith "extension method calls not implemented"
  end
and sig_ok (ctx:context) (st:sig_t) : bool =
  let () = if wDEBUG then Format.printf "[sig_ok] '%s'\n" (string_of_sig_t st) in
  begin match st with
  | C ct -> class_ok ctx ct
  | I it -> interface_ok ctx it
  end
(* [type_ok ?sel ctx tt] Check that the type [tt] is valid in context [ctx].
   I think this is just 'are all variables bound?' but idk... TODO *)
and type_ok (ctx:context) (tt:type_t) : bool =
  let () = if wDEBUG then Format.printf "[type_ok] '%s'\n" (string_of_type_t tt) in
  let class_c, _, _ = ctx in
  begin match tt with
  | Top | Bot -> true
  | TVar v | Super v -> param_ok ctx v
  | Instance(name, vm) -> StringMap.mem name class_c
  end

let rec method_names (ctx:context) (tt:type_t) : string list =
  let () = if wDEBUG then Format.printf "[method_names] '%s'\n" (string_of_type_t tt) in
  begin match tt with
  | Top -> []
  | Bot -> ["Bot=>All"]
  | TVar v | Super v -> method_names ctx (lookup_tau_o ctx v)
  | Instance(name, vm) ->
     let ms =
       begin match lookup_class ctx name with
       | C (Class(_,_,_,_,_,ms)) -> List.map (fun (a,(b,c)) -> (a,b)) ms
       | I (Interface(_,_,_,_,ms)) -> ms
       end
     in
     let ctx' = update_vars vm ctx in
     List.map string_of_method_t (filter_by_condition ctx' ms)
  end
