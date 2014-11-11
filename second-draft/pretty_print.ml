open Definitions

let rec string_of_type_t_shallow (ctx:Context.t) (tt:type_t) : string =
  begin match tt with
  | TVar s -> s
  | Super s -> "super " ^ s
  | Instance(s, tc) ->
     let params =
       begin match Context.find_sig ctx s with
       | C (Class(_,params,_,_,_,_))   -> params
       | I (Interface(_,params,_,_,_)) -> params
       end
     in
     let ctx' = Context.add_vars ctx tc in
     let param_strs = List.map (fun p -> string_of_type_t_shallow ctx (Context.find_tau_o ctx' p)) params in
     Format.sprintf "%s<%s>" s (String.concat ", " param_strs)
  | Top -> "Top"
  | Bot -> "Bot"
  end

let string_of_shape_t_shallow (ctx:Context.t) (st:shape_t) : string =
  let Shape(name, _, _) = st in
  name

let string_of_interface_t_shallow (ctx:Context.t) (it:inter_t) : string =
  let Interface(name, params, _, _, _) = it in
  let param_strs = List.map (fun p -> string_of_type_t_shallow ctx (Context.find_tau_o ctx p)) params in
  Format.sprintf "%s<%s>"
                 name
                 (String.concat ", " param_strs)

let string_of_class_t_shallow (ctx:Context.t) (ct:class_t) : string =
  let Class(name, params, _, _, _, _) = ct in
  let param_strs = List.map (fun p -> string_of_type_t_shallow ctx (Context.find_tau_o ctx p)) params in
  Format.sprintf "%s<%s>"
                 name
                 (String.concat ", " param_strs)

let string_of_cond_t (ctx:Context.t) (f:'a -> string) ((c,b):(cond_t * 'a)) : string =
  let str = f b in
  begin match c with
  | NoCond          -> str
  | Sat(s,shp)      ->
     Format.sprintf "[[%s satisfies %s]] %s"
                    s
                    (string_of_shape_t_shallow ctx shp)
                    str
  | SuperSat(s,shp) ->
     Format.sprintf "[[super %s satisfies %s]] %s"
                    s
                    (string_of_shape_t_shallow ctx shp)
                    str
  end

let string_of_extends (ctx:Context.t) (exts:(cond_t * class_t)  list) : string =
  begin match exts with
  | [] -> ""
  | _  ->
     let ext_strs = List.map (string_of_cond_t ctx (string_of_class_t_shallow ctx)) exts in
     Format.sprintf "\n    extends %s"
                    (String.concat ", " ext_strs)
  end

let string_of_implements (ctx:Context.t) (impls:(cond_t * inter_t) list) : string =
  begin match impls with
  | [] -> ""
  | _  ->
     let impl_strs = List.map (string_of_cond_t ctx (string_of_interface_t_shallow ctx)) impls in
     Format.sprintf "\n    implements %s"
                    (String.concat ", " impl_strs)
  end

let string_of_shapes (ctx:Context.t) (shps:(cond_t * shape_t) list) : string =
  begin match shps with
  | [] -> ""
  | _  ->
     let shp_strs = List.map (string_of_cond_t ctx (string_of_shape_t_shallow ctx)) shps in
     Format.sprintf "\n    satisfies %s"
                    (String.concat ", " shp_strs)
  end

let string_of_arg_t (ctx:Context.t) (a:arg_t) : string =
  let Arg(tt, s) = a in
  Format.sprintf "%s %s"
                 (string_of_type_t_shallow ctx tt)
                 s

let string_of_stmt_t (ctx:Context.t) (b:stmt_t) : string =
  begin match b with
  | Return Null ->
     "return null;"
  | Return (New(cname,tc)) ->
     Format.sprintf "return (new %s);"
                    (string_of_type_t_shallow ctx (Instance(cname, tc)))
  | Return (Call((cname,tc),mname,vals)) ->
     (* 2014-11-10: I am so sorry about this indentation. (bg) *)
     Format.sprintf "return %s.%s(%s);"
                    (string_of_type_t_shallow ctx (Instance(cname,tc)))
                    mname
                    (String.concat ", "
                                   (List.map (string_of_type_t_shallow ctx)
                                             (List.map (fun (a,b) -> Instance(a,b))
                                                       vals)))
  | Return (ExtM (cname,mname,args)) ->
     "return ERROR:not.implemented();"
  end

let string_of_method_impl (ctx:Context.t) ((m,b):(method_t * stmt_t)) : string =
  let Method(rtype, name, args) = m in
  let ctx' = Context.flip_variance ctx in
  Format.sprintf "%s %s (%s) {\n    %s\n  }"
                 (string_of_type_t_shallow ctx rtype)
                 name
                 (String.concat ", " (List.map (string_of_arg_t ctx') args))
                 (string_of_stmt_t ctx b)

let string_of_method_sig (ctx:Context.t) (m:method_t) : string =
  let Method(rtype, name, args) = m in
  let ctx' = Context.flip_variance ctx in
  Format.sprintf "%s %s (%s);"
                 (string_of_type_t_shallow ctx rtype)
                 name
                 (String.concat ", " (List.map (string_of_arg_t ctx') args))

let indent (str:string) : string =
  "  " ^ str

let string_of_method_impls (ctx:Context.t) (mthds:(cond_t * (method_t * stmt_t)) list) : string =
  String.concat "\n"
                (List.map (fun m ->
                           indent (string_of_cond_t ctx (string_of_method_impl ctx) m))
                          mthds)

let string_of_method_sigs (ctx:Context.t) (mthds:(cond_t * method_t) list) : string =
  String.concat "\n"
                (List.map (fun m ->
                           indent (string_of_cond_t ctx (string_of_method_sig  ctx) m))
                          mthds)

let string_of_shape_t (ctx:Context.t) (st:shape_t) : string =
  let Shape(name,shps,mthds) = st in
  Format.sprintf "shape %s%s {\n%s\n}\n"
                 name
                 (string_of_shapes      ctx shps)
                 (string_of_method_sigs ctx mthds)

let string_of_interface_t (ctx:Context.t) (it:inter_t) : string =
  let Interface(name,params,impls,shps,mthds) = it in
  Format.sprintf "interface %s<%s>%s%s {\n%s\n}\n"
                 name
                 (String.concat ", " params)
                 (string_of_implements  ctx impls)
                 (string_of_shapes      ctx shps)
                 (string_of_method_sigs ctx mthds)

let string_of_class_t (ctx:Context.t) (ct:class_t) : string =
  let Class(name,params,exts,impls,shps,mthds) = ct in
  Format.sprintf "class %s<%s>%s%s%s {\n%s\n}\n"
                 name
                 (String.concat ", " params)
                 (string_of_extends      ctx exts)
                 (string_of_implements   ctx impls)
                 (string_of_shapes       ctx shps)
                 (string_of_method_impls ctx mthds)

let string_of_sig_t (ctx:Context.t) (st:sig_t) : string =
  begin match st with
  | C c -> string_of_class_t ctx c
  | I i -> string_of_interface_t ctx i
  end
