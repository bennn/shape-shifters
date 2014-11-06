let debug = false

(*** typedefs ***)
type  cname    = string
and  cfield   = Field of ctype * cname
and  carg     = Arg   of ctype * cname
(* just signatures, the body doesn't matter *)
and  cmethod  = Method of ctype * cname * carg list
(* and  cstmt    = Return of cexpr(\* Seq of stmt list | Assign of  *\) *)
(* and  cexpr    = Var of cname | New of class_record | Null *)
and  class_sig = { name : cname
                 ; param : cname
                 ; extends : (predicate* vclass)
                 ; implements : (predicate * vinterface) option
                 ; satisfies : (predicate * vshape) option
                 ; fields : (predicate * cfield) list
                 ; methods : (predicate * cmethod) list
                 }
and interface_sig = { iname : cname
                    ; iparam : cname
                    ; iextends : (predicate * vinterface) option
                    ; isatisfies : (predicate * vshape) option
                    ; ifields : (predicate * cfield) list
                    ; imethods : (predicate * cmethod) list
                    }
and shape_sig = { sname : cname
                ; sextends : (predicate * vshape) option
                ; sfields : (predicate * cfield) list
                ; smethods : (predicate * cmethod) list
                }
(* instantiation is a Class + ti + to *)
and class_record  = { cls   : class_sig
                    ; tau_i : vclass
                    ; tau_o : vclass
                    }
and interface_record  = { intr   : interface_sig
                        ; itau_i : vclass
                        ; itau_o : vclass
                        }
and shape_record = { shp : shape_sig
                   ; stau_i : vclass
                   ; stau_o : vclass
                   }
and predicate = Satisfies of vshape | Nil
and vclass = Class of class_record | Top | Bot
(* not top/bot for interfaces, but maybe there should be *)
(* actually, Top/Bot should be ctypes, probably *)
and vinterface = Interface of interface_record
and vshape = Shape of shape_record
and  ctype    = TVar of string | CType of vclass | IType of vinterface

(*** misc ***)
let and_all xs = List.fold_left (fun acc x -> acc && x) true  xs
let record_of_vclass_exn (c:vclass) : class_record =
  match c with
  | Top | Bot -> failwith "record_of_vclass_exn"
  | Class cr  -> cr

let string_of_vclass (vc:vclass) : string =
  match vc with
  | Top -> "Top"
  | Bot -> "Bot"
  | Class cr -> cr.cls.name

(*** context ***)
(* Kind Context maps type variables *)
type kind_context = (string * ctype * ctype) list
(* 2014-10-29 the whole context might have more parts, later *)
type context = kind_context
let empty_context = []
let rec context_lookup (k:cname) (ctx:context) : ctype =
  match ctx with
  | [] -> failwith (Format.sprintf "unbound variable '%s'" k)
  | (k',_,tau_o)::tl -> if k = k' then tau_o else context_lookup k tl
let rec context_add (k:cname) (tau_i:ctype) (tau_o:ctype) (ctx:context) =
  match ctx with
  | [] -> (k, tau_i, tau_o) :: ctx
  | (k',t',t'') :: tl ->
     let rst = context_add k tau_i tau_o tl in
     if k = k'
     then failwith (Format.sprintf "[context_add] DUPLICATE KEY '%s'" k)
     else (k',t',t'') :: rst
let rec context_mem k ctx =
  match ctx with
  | [] -> false
  | (k',_,_)::tl -> k = k' || context_mem k tl
let rec context_remove k ctx =
  match ctx with
  | [] -> []
  | (k',_,_)::tl when k = k' -> tl
  | hd::tl -> hd :: context_remove k tl
let rec context_flip ctx =
  match ctx with
  | [] -> []
  | (k, tau_i, tau_o) :: tl -> (k, tau_o, tau_i) :: context_flip tl

let rec subst (hole:vclass) (tau_i':vclass) (tau_o':vclass) : vclass =
  (* should this substitute in the extends/implements? *)
  let () = Format.printf "[subst] hole is '%s'\n" (string_of_vclass hole) in
  begin
    match hole with
    | Top -> Top
    | Bot -> Bot
    | Class cr -> Class {cls = cr.cls
                        ; tau_i = (subst cr.tau_i tau_o' tau_i')
                        ; tau_o = (subst cr.tau_o tau_i' tau_o')
                        }
  end

let rec vclass_of_ctype (c:ctype) (ctx:context) : vclass option =
  begin match c with
    | TVar k  -> vclass_of_ctype (context_lookup k ctx) (context_remove k ctx)
    | CType vc -> Some vc
    | IType vi -> None
  end
let rec vinterface_of_ctype (c:ctype) (ctx:context) : vinterface option =
  begin match c with
    | TVar k  -> vinterface_of_ctype (context_lookup k ctx) (context_remove k ctx)
    | CType vc -> None
    | IType vi -> Some vi
  end

(*** inheritance + subtyping ***)
(* record-only version would avoid some checks *)
let rec inherits (c:vclass) (d:vclass) : vclass option =
  begin match c,d with
  | Bot, Bot -> None
  | Bot, Class d' -> (* funky *) Some (Class d')
  | Bot, Top -> failwith "[<::] tricky case, bot <:: top. IDK"
  | Top, _ -> None
  | Class _ , Bot -> None
  | Class _ , Top -> failwith "[<::] tricky case, CLS <:: TOP. idk"
  | Class c', Class d' when c'.cls.name = d'.cls.name -> None
  | Class c', Class d' ->
     let () = if debug then Format.printf "[inherit] checking '%s' <:: '%s'\n" c'.cls.name d'.cls.name in
     if cpredicate_ok c'.tau_o (fst c'.cls.extends) then
       begin match snd c'.cls.extends with
       | Top -> None (* c extends Top *)
       | Bot -> failwith "[inherits] found a malfored class"
       | Class cr when cr.cls.name = d'.cls.name -> Some (Class cr)
       | Class cr -> inherits (Class cr) d
       end
     else None
  end
and inherits_eq (c:vclass) (d:vclass) : vclass option =
  begin
    match c,d with
    | Bot, Bot -> failwith "[<=::] tricky case, bot bot"
    | Top, Top -> failwith "[<=::] tricky case, top top"
    | Class c', Class d' when c'.cls.name = d'.cls.name -> Some (Class c')
    | _, _ -> inherits c d
  end

and i_inherits (i1:vinterface) (i2:vinterface) : vinterface option =
  let (Interface ir1, Interface ir2) = i1, i2 in
  if ir1.intr.iname = ir2.intr.iname
  then None
  else
    let () = if debug then Format.printf "[i_inherit] checking '%s' <:: '%s'\n" ir1.intr.iname ir2.intr.iname in
      begin match ir1.intr.iextends with
      | None -> None
      | Some (p, Interface ir1') ->
         if cpredicate_ok (ir1.itau_o) p then (
           if ir1'.intr.iname = ir2.intr.iname
           then Some (Interface ir1')
           else i_inherits (Interface ir1') i2
         )else None
      end
and i_inherits_eq (i1:vinterface) (i2:vinterface) : vinterface option =
  let (Interface ir1, Interface ir2) = i1, i2 in
  if ir1.intr.iname = ir2.intr.iname
  then Some i1
  else i_inherits i1 i2

and s_inherits (s1:vshape) (s2:vshape) : vshape option =
  let (Shape sr1, Shape sr2) = s1, s2 in
  if sr1.shp.sname = sr2.shp.sname then None
  else
    let () = if debug then Format.printf "[s_inherit] checking '%s' <:: '%s'\n" sr1.shp.sname sr2.shp.sname in
    begin match sr1.shp.sextends with
    | None -> None
    | Some (p, Shape sr1') ->
       if cpredicate_ok (sr1.stau_o) p then (
         if sr1'.shp.sname = sr2.shp.sname
         then Some (Shape sr1')
         else s_inherits (Shape sr1') s2
       )else None
    end
and s_inherits_eq (s1:vshape) (s2:vshape) : vshape option =
  let (Shape sr1, Shape sr2) = s1, s2 in
  if sr1.shp.sname = sr2.shp.sname
  then Some s1
  else s_inherits s1 s2

and ccsubtype (c:vclass) (d:vclass) : bool =
  begin match c, d with
  | Bot, _ -> true
  | _, Top -> true
  | Class c', Class d' ->
       let () = if debug then Format.printf "[ccsubtype] checking '%s' <: '%s'\n" c'.cls.name d'.cls.name in
       let c_inhr = inherits_eq c d in
       (* if c_opt is none, return false *)
       begin match c_inhr with
       | None -> false
       | Some Bot -> failwith "[ccsubtype] found malformed class inheriting Bot"
       | Some Top -> failwith "[ccsubtype] should never reach this TOP case"
       | Some (Class vc) ->
          (* Check subtyping on both parameters *)
          let tau_i_okay = ccsubtype d'.tau_i
                                     (subst vc.tau_i c'.tau_o c'.tau_i) in
          let tau_o_okay = ccsubtype (subst vc.tau_o c'.tau_i c'.tau_o)
                                     d'.tau_o in
          (&&) tau_i_okay tau_o_okay
       end
  | _, _ -> false
  end

and iisubtype (i1:vinterface) (i2:vinterface) : bool =
  let (Interface ir1, Interface ir2) = i1 , i2 in
  let i_inhr = i_inherits_eq i1 i2 in
  begin match i_inhr with
        | None -> false
        | Some (Interface ir') ->
           let tau_i_okay = ccsubtype ir2.itau_i
                                      (subst ir'.itau_i ir1.itau_o ir1.itau_i) in
           let tau_o_okay = ccsubtype (subst ir'.itau_o ir1.itau_i  ir1.itau_o)
                                      ir2.itau_o in
           (&&) tau_i_okay tau_o_okay
  end

and cisubtype (c1:vclass) (i2:vinterface) : bool =
  begin match c1 with
  | Bot -> true
  | Top -> false
  | Class cr ->
     (* If directly implements or implements something that extends it *)
     (* let  = implements_eq c1 i2 in *)
     failwith "cisubtype not implemented"
  end

and subtype (ctx:context) (c1:ctype) (c2:ctype) : bool =
  let class1 = vclass_of_ctype c1 ctx in
  let intr1  = vinterface_of_ctype c1 ctx in
  let class2 = vclass_of_ctype c2 ctx in
  let intr2  = vinterface_of_ctype c2 ctx in
  begin match (class1, intr1, class2, intr2) with
  | (Some vc1, None    , Some vc2, None    ) -> ccsubtype vc1 vc2
  | (None    , Some vi1, None    , Some vi2) -> iisubtype vi1 vi2
  | (Some vc1, None    , None    , Some vi2) -> cisubtype vc1 vi2
  | _ -> false
  end

and subtype_method (ctx:context) (m1:cmethod) (m2:cmethod) : bool =
  let Method (r1, n1, args1) = m1 in
  let Method (r2, n2, args2) = m2 in
  (* m1.returntype <: m2.returntype *)
  let r_ok = subtype ctx r1 r2 in
  (* forall args, m2.arg <: m1.arg *)
  let ctx' = context_flip ctx in
  let a_ok =
    ((List.length args1) = (List.length args2))
    && (List.fold_left2 (fun acc (Arg (a1,_)) (Arg (a2,_)) ->
                         acc && subtype ctx' a2 a1)
                       true args1 args2)
  in
  and_all [r_ok; a_ok]

and sssubtype (s1:shape_record) (s2:shape_record) : bool =
  let s_inhr = s_inherits_eq (Shape s1) (Shape s2) in
  begin match s_inhr with
  | None -> false
  | Some (Shape sr') ->
     let tau_i_okay = ccsubtype s2.stau_i
                                (subst sr'.stau_i s1.stau_o s1.stau_i) in
     let tau_o_okay = ccsubtype (subst sr'.stau_o s1.stau_i s1.stau_o)
                                s2.stau_o in
     (&&) tau_i_okay tau_o_okay
  end

and cpredicate_ok (vc:vclass) (pred:predicate) : bool =
  match vc with
  | Top | Bot -> false
  | Class cr ->
  begin match pred with
  | Nil -> true
  | Satisfies (Shape s) ->
     begin match cr.cls.satisfies with
     | None -> false
     | Some (p', Shape sr) ->
        begin match cr.tau_o with
        | Top | Bot -> false
        | Class cr' -> if cpredicate_ok (Class cr') p' then
                         (* check that vs <: s *)
                         (* really need to subst... ugh *)
                         sssubtype sr s
                       else false
        end
     end
  end

and ipredicate_ok (ir:interface_record) (pred:predicate) : bool =
  begin match pred with
  | Nil -> true
  | Satisfies (Shape s) ->
     begin match ir.intr.isatisfies with
     | None -> false
     | Some (p', (Shape sr)) ->
        begin match ir.itau_o with
        | Top | Bot -> false
        | Class cr' -> if cpredicate_ok (Class cr') p' then
                         sssubtype sr s
                       else false
        end
     end
  end

and spredicate_ok (sr:shape_record) (pred:predicate) : bool =
  begin match pred with
  | Nil -> true
  | Satisfies (Shape s) ->
     begin match sr.shp.sextends with
     | None -> false
     | Some (p', (Shape sr)) ->
        begin match sr.stau_o with
        | Top | Bot -> false
        | Class cr' -> if cpredicate_ok (Class cr') p' then
                         sssubtype sr s
                       else false
        end
     end
  end

(*** collections ***)
module MethodSet = Set.Make (struct
  type t = predicate * cmethod
  let compare (p1, (Method (_,n1,_))) (p2, (Method (_,n2,_))) = Pervasives.compare n1 n2
end)
(* Build a set of current & super methods -- these are what the current class might possibly override *)
let rec collect_extends_methods_aux (acc:MethodSet.t) (c:vclass) : MethodSet.t =
  begin
    match c with
    | Top | Bot -> acc
    | Class cr  ->
       (* Decide predicate *)
       if cpredicate_ok (Class cr) (fst cr.cls.extends) then
         (* it holds, recurse *)
         collect_extends_methods_aux
           (MethodSet.union acc (MethodSet.of_list cr.cls.methods))
           (snd cr.cls.extends)
       else acc (* done! *)
  end
let collect_extends_methods (c:vclass) : MethodSet.t =
  collect_extends_methods_aux MethodSet.empty c

let rec collect_implements_methods_aux (acc:MethodSet.t) (v:vinterface option) : MethodSet.t =
  begin match v with
  | None -> acc
  | Some (Interface ir) ->
     begin match ir.intr.iextends with
     | None -> acc
     | Some (p, Interface ir') ->
        if ipredicate_ok ir p then
          collect_implements_methods_aux
            (MethodSet.union acc (MethodSet.of_list ir.intr.imethods))
            (Some (Interface ir'))
        else acc
     end
  end
let collect_implements_methods (v:vinterface option) : MethodSet.t =
  collect_implements_methods_aux MethodSet.empty v

let rec collect_satisfies_methods_aux (acc:MethodSet.t) (s:vshape option) : MethodSet.t =
  begin match s with
  | None -> acc
  | Some (Shape sr) ->
     begin match sr.shp.sextends with
     | None -> acc
     | Some (p, Shape sr') ->
        if spredicate_ok sr p then
          collect_satisfies_methods_aux
            (MethodSet.union acc (MethodSet.of_list sr.shp.smethods))
            (Some (Shape sr'))
        else acc
     end
  end

let collect_satisfies_methods (s:vshape option) : MethodSet.t =
  collect_satisfies_methods_aux MethodSet.empty s

(*** well-formedness ***)
(* trivial now, may want to check uniqueness later *)
let param_ok (ctx:context) (c:class_record) : bool =
  true
let iparam_ok (ctx:context) (c:interface_record) : bool =
  true
let rec class_ok_aux (ctx:context) (c:vclass) : bool =
  begin
    match c with
    | Top | Bot -> true
    | Class c' ->
       (* bind param to tau_i tau_o *)
       let ctx' = context_add (c'.cls.param) (CType c'.tau_i) (CType c'.tau_o) ctx in
       let p_ok = param_ok ctx' c' in
       let e_ok = TODO TODO TODO
         class_ok_aux ctx' (c'.cls.extends)
       in
       let f_ok = and_all (List.map (field_ok ctx') (c'.cls.fields)) in
       let m_ok = methods_ok ctx' c' in
       and_all [p_ok; e_ok; f_ok; m_ok]
  end
and interface_ok_aux (ctx:context) (i:vinterface) : bool =
  begin
    match i with
    | Interface ir ->
       let ctx' = context_add (ir.intr.iparam) (CType ir.itau_i) (CType ir.itau_o) ctx in
       let p_ok = iparam_ok ctx' ir in
       let e_ok =
         match ir.intr.iextends with
         | None -> true
         | Some i -> interface_ok_aux ctx' i
       in
       let f_ok = and_all (List.map (field_ok ctx') (ir.intr.ifields)) in
       let m_ok = imethods_ok ctx' ir in
       and_all [p_ok; e_ok; f_ok; m_ok]
  end
and shape_ok_aux (ctx:context) (s:vshape) : bool =
  match s with
  | Shape sr ->
     let e_ok =
       match sr.shp.sextends with
       | None -> true
       | Some s' -> shape_ok_aux ctx s'
     in
     let f_ok = and_all (List.map (field_ok ctx) sr.shp.sfields) in
     let m_ok = smethods_ok ctx sr in
     and_all [e_ok; f_ok; m_ok]
and arg_ok (ctx:context) (a:carg) : bool =
  match a with | Arg (t, name) -> type_ok ctx t
and body_ok (ctx:context) : bool =
  true (* TODO want to allow implementations, eventually *)
and field_ok (ctx:context) (f:cfield) : bool =
  match f with | Field (t, name) -> type_ok ctx t
and imethods_ok (ctx:context) (ir:interface_record) : bool =
  let extm_set = collect_implements_methods ir.intr.iextends in
  let im_set   = MethodSet.of_list ir.intr.imethods in
  (* No dups! *)
  let no_dups  = (=) (List.length ir.intr.imethods) (MethodSet.cardinal im_set) in
  (* Each method typechecks *)
  let types_ok = List.fold_left
    (fun acc m ->
     let Method (ret_type, name, args) = m in
     acc
     && type_ok ctx ret_type
     && (if MethodSet.mem m extm_set
         then subtype_method ctx m (MethodSet.find m extm_set)
         else true)
    ) true ir.intr.imethods
  in
  and_all [no_dups; types_ok]
and smethods_ok (ctx:context) (sr:shape_record) : bool =
  let extm_set = collect_satisfies_methods sr.shp.sextends in
  let sm_set = MethodSet.of_list sr.shp.smethods in
  (* no duplicate methods *)
  let no_dups = (=) (List.length sr.shp.smethods) (MethodSet.cardinal sm_set) in
  (* all methods well-typed with respect to parent *)
  let types_ok = List.fold_left
    (fun acc m ->
     let Method (ret_type, name, args) = m in
     acc
     && type_ok ctx ret_type
     && (if MethodSet.mem m extm_set
         then subtype_method ctx m (MethodSet.find m extm_set)
         else true)
    ) true sr.shp.smethods
  in
  and_all [no_dups; types_ok]
and methods_ok (ctx:context) (c:class_record) : bool =
  (* All methods we possibly overwrite *)
  let extm_set = collect_extends_methods c.cls.extends in
  (* All methods we need to implement. That is, all interface methods minus inherited methods. *)
  let intm_set = MethodSet.diff (collect_implements_methods c.cls.implements) extm_set in
  let satm_set = MethodSet.diff (collect_satisfies_methods c.cls.satisfies) extm_set in
  let clsm_set = MethodSet.of_list c.cls.methods in
  (* CHECK CONDITIONS *)
  (* First, no duplicates *)
  let no_duplicates = (=) (List.length c.cls.methods)
                          (MethodSet.cardinal clsm_set)
  in
  (* Next, all interfaces implemented *)
  let interfaces_implemented = MethodSet.is_empty (MethodSet.diff intm_set clsm_set) in
  let shapes_satisfied = MethodSet.is_empty (MethodSet.diff satm_set clsm_set) in
  (* Next, all methods well-typed *)
  let types_ok = List.fold_left
    (fun acc m ->
     let Method (ret_type, name, args) = m in
     acc
     && type_ok ctx ret_type
     && and_all (List.map (arg_ok (context_flip ctx)) args)
     && (if MethodSet.mem m extm_set
         then subtype_method ctx m (MethodSet.find m extm_set)
         else true)
     && (if MethodSet.mem m intm_set
         then subtype_method ctx m (MethodSet.find m intm_set)
         else true)
     && (if MethodSet.mem m satm_set
         then subtype_method ctx m (MethodSet.find m satm_set)
         else true)
    ) true c.cls.methods
  in
  and_all [no_duplicates; interfaces_implemented; shapes_satisfied; types_ok]
and type_ok (ctx:context) (t:ctype) : bool =
  match t with
  | TVar str -> context_mem str ctx
  | CType c -> class_ok_aux ctx c
  | IType i -> interface_ok_aux ctx i

let class_ok (c:vclass) : bool =
  class_ok_aux empty_context c
let interface_ok (i:vinterface) : bool =
  interface_ok_aux empty_context i
let shape_ok (s:vshape) : bool =
  shape_ok_aux empty_context s
