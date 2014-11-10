open Definitions

let rec ptype_shallow (ctx:context) (tt:type_t) : string =
  begin match tt with
  | TVar s -> s
  | Super s -> "super " ^ s
  | Instance(s, vm) ->
     let params =
       begin match lookup_class ctx s with
       | C (Class(_,params,_,_,_,_))   -> params
       | I (Interface(_,params,_,_,_)) -> params
       end
     in
     let ctx' = context_addvarmap ctx vm in
     let param_strs = List.map (fun p -> ptype_shallow ctx (lookup_tau_o ctx' p)) params in
     Format.sprintf "%s<%s>" s (String.concat ", " param_strs)
  | Top -> "Top"
  | Bot -> "Bot"
  end

let pshape_shallow (ctx:context) (st:shape_t) : string =
  let Shape(name, _, _) = st in
  name

let pinter_shallow (ctx:context) (it:inter_t) : string =
  let Interface(name, params, _, _, _) = it in
  Format.sprintf "%s<%s>"
                 name
                 (String.concat ", " (List.map (fun p -> ptype_shallow ctx (lookup_tau_o ctx p))
                                               params))

let pclass_shallow (ctx:context) (ct:class_t) : string =
  let Class(name, params, _, _, _, _) = ct in
  Format.sprintf "%s<%s>"
                 name
                 (String.concat ", " (List.map (fun p -> ptype_shallow ctx (lookup_tau_o ctx p))
                                               params))

let pcond (ctx:context) (f:'a -> string) ((c,b):(cond_t * 'a)) : string =
  let str = f b in
  begin match c with
  | NoCond -> str
  | Sat(s,shp) -> Format.sprintf "[[%s satisfies %s]] %s" s (pshape_shallow ctx shp) str
  | SuperSat(s,shp) -> Format.sprintf "[[super %s satisfies %s]] %s" s (pshape_shallow ctx shp) str
  end

let pextends (ctx:context) (exts:(cond_t * class_t)  list) : string =
  begin match exts with
  | [] -> ""
  | _  -> Format.sprintf " extends %s"
                         (String.concat ", "
                                        (List.map (pcond ctx (pclass_shallow ctx)) exts))
  end

let pimpls (ctx:context) (impls:(cond_t * inter_t) list) : string =
  begin match impls with
  | [] -> ""
  | _  -> Format.sprintf " implements %s"
                         (String.concat ", "
                                        (List.map (pcond ctx (pinter_shallow ctx)) impls))
  end

let pshps (ctx:context) (shps:(cond_t * shape_t) list) : string =
  begin match shps with
  | [] -> ""
  | _  -> Format.sprintf " satisfies %s"
                         (String.concat ", "
                                        (List.map (pcond ctx (pshape_shallow ctx)) shps))
  end

let parg (ctx:context) (a:arg_t) : string =
  let Arg(tt, s) = a in
  Format.sprintf "%s %s" (ptype_shallow ctx tt) s

let pbody (ctx:context) (b:stmt_t) : string =
  begin match b with
  | Return Null -> "return null;"
  | Return (New(cname,vm)) -> Format.sprintf "return (new %s);" (ptype_shallow ctx (Instance(cname, vm)))
  | Return (Call((cname,vm),mname,vals)) ->
     Format.sprintf "return %s.%s(%s);"
                    (ptype_shallow ctx (Instance(cname,vm)))
                    mname
                    (String.concat ", " (List.map (ptype_shallow ctx) (List.map (fun (a,b) -> Instance(a,b)) vals)))
  | Return (ExtM (cname,mname,args)) -> "return ERROR:not.implemented();"
  end

let pmethod_impl (ctx:context) ((m,b):(method_t * stmt_t)) : string =
  let Method(rtype, name, args) = m in
  let ctx' = flip_variance ctx in
  Format.sprintf "%s %s (%s) {\n    %s\n  }"
                 (ptype_shallow ctx rtype)
                 name
                 (String.concat ", " (List.map (parg ctx') args))
                 (pbody ctx b)

let pmethod_sig (ctx:context) (m:method_t) : string =
  let Method(rtype, name, args) = m in
  let ctx' = flip_variance ctx in
  Format.sprintf "%s %s (%s);"
                 (ptype_shallow ctx rtype)
                 name
                 (String.concat ", " (List.map (parg ctx') args))

let pmethod_impls (ctx:context) (mthds:(cond_t * (method_t * stmt_t)) list) : string =
  String.concat "\n" (List.map (fun m -> "  " ^ pcond ctx (pmethod_impl ctx) m) mthds)

let pmethod_sigs (ctx:context) (mthds:(cond_t * method_t) list) : string =
  String.concat "\n" (List.map (fun m -> "  " ^ pcond ctx (pmethod_sig ctx) m) mthds)

let pshape_t (ctx:context) (st:shape_t) : string =
  let Shape(name,shps,mthds) = st in
  Format.sprintf "shape %s%s {\n%s\n}\n"
                 name
                 (pshps ctx shps)
                 (pmethod_sigs ctx mthds)

let pclass_t (ctx:context) (ct:class_t) : string =
  let Class(name,params,exts,impls,shps,mthds) = ct in
  Format.sprintf "class %s<%s>%s%s%s {\n%s\n}\n"
                 name
                 (String.concat ", " params)
                 (pextends ctx exts)
                 (pimpls   ctx impls)
                 (pshps    ctx shps)
                 (pmethod_impls ctx mthds)


let pinter_t (ctx:context) (it:inter_t) : string =
  let Interface(name,params,impls,shps,mthds) = it in
  Format.sprintf "interface %s<%s>%s%s {\n%s\n}\n"
                 name
                 (String.concat ", " params)
                 (pimpls ctx impls)
                 (pshps ctx shps)
                 (pmethod_sigs ctx mthds)
