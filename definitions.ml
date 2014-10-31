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
                 ; extends : vclass
                 ; implements : vinterface option
                 (* ; satisfies : cshape *)
                 ; fields : cfield list
                 ; methods : cmethod list
                 }
(* instantiation is a Class + ti + to *)
and class_record  = { cls   : class_sig
                    ; tau_i : vclass
                    ; tau_o : vclass
                    }
and vclass = Class of class_record | Top | Bot
and interface_sig = { iname : cname
                    ; iparam : cname
                    ; iextends : vinterface option
                    ; ifields : cfield list
                    ; imethods : cmethod list
                    }
(* not top/bot for interfaces, but maybe there should be *)
(* actually, Top/Bot should be ctypes, probably *)
and interface_record  = { intr   : interface_sig
                        ; itau_i : vclass
                        ; itau_o : vclass
                        }
and vinterface = Interface of interface_record
and  ctype    = TVar of string | CType of vclass | IType of vinterface

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
let rec context_add k tau_i tau_o ctx =
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
  begin
    match hole with
    | Top -> Top
    | Bot -> Bot
    | Class cr -> Class {cls = cr.cls
                        ; tau_i = (subst cr.tau_i tau_o' tau_i')
                        ; tau_o = (subst cr.tau_o tau_i' tau_o')
                        }
  end

let rec vclass_of_ctype (c:ctype) (ctx:context) : vclass =
  begin match c with
    | TVar k  -> vclass_of_ctype (context_lookup k ctx) (context_remove k ctx)
    | CType vc -> vc
    | IType vi -> failwith "vclass_of_ctype"
  end
let rec vinterface_of_ctype (c:ctype) (ctx:context) : vinterface =
  begin match c with
    | TVar k  -> vinterface_of_ctype (context_lookup k ctx) (context_remove k ctx)
    | CType vc -> failwith "vinterface_of_ctype"
    | IType vi -> vi
  end

(*** collections ***)
module MethodSet = Set.Make (struct
  type t = cmethod
  let compare (Method (_,n1,_)) (Method (_,n2,_)) = Pervasives.compare n1 n2
end)
(* Build a set of current & super methods -- these are what the current class might possibly override *)
let rec collect_extends_methods_aux (acc:MethodSet.t) (c:vclass) : MethodSet.t =
  begin
    match c with
    | Top | Bot -> acc
    | Class cr  -> collect_extends_methods_aux
                     (MethodSet.union acc (MethodSet.of_list cr.cls.methods))
                     cr.cls.extends
  end
let collect_extends_methods (c:vclass) : MethodSet.t =
  collect_extends_methods_aux MethodSet.empty c

let rec collect_implements_methods_aux (acc:MethodSet.t) (v:vinterface option) : MethodSet.t =
  begin match v with
  | None -> acc
  | Some (Interface ir) -> collect_implements_methods_aux
                             (MethodSet.union acc (MethodSet.of_list ir.intr.imethods))
                             ir.intr.iextends
  end
let collect_implements_methods (v:vinterface option) : MethodSet.t =
  collect_implements_methods_aux MethodSet.empty v

(*** misc ***)
let and_all xs = List.fold_left (fun acc x -> acc && x) true  xs
let record_of_vclass_exn (c:vclass) : class_record =
  match c with
  | Top | Bot -> failwith "record_of_vclass_exn"
  | Class cr  -> cr
(* let or_all  f xs = List.fold_left (fun acc x -> acc || f x) false xs *)

(*** inheritance + subtyping ***)
(* record-only version would avoid some checks *)
let rec inherits (c:vclass) (d:vclass) : vclass option =
  begin
    match c,d with
    | Bot, Bot -> None
    | Bot, Class d' -> (* funky *) Some (Class d')
    | Bot, Top -> failwith "[<::] tricky case, bot <:: top. IDK"
    | Top, _ -> None
    | Class _ , Bot -> None
    | Class _ , Top -> failwith "[<::] tricky case, CLS <:: TOP. idk"
    | Class c', Class d' when c'.cls.name = d'.cls.name -> None
    | Class c', Class d' ->
       let () = if debug then Format.printf "[inherit] checking '%s' <:: '%s'\n" c'.cls.name d'.cls.name in
       begin
         match c'.cls.extends with
         | Top -> None (* c extends Top *)
         | Bot -> failwith "[inherits] found a malfored class"
         | Class cr when cr.cls.name = d'.cls.name -> Some (Class cr)
         | Class cr -> inherits c'.cls.extends d
       end
  end

let inherits_eq (c:vclass) (d:vclass) : vclass option =
  begin
    match c,d with
    | Bot, Bot -> failwith "[<=::] tricky case, bot bot"
    | Top, Top -> failwith "[<=::] tricky case, top top"
    | Class c', Class d' when c'.cls.name = d'.cls.name -> Some (Class c')
    | _, _ -> inherits c d
  end

let rec subtype (c:vclass) (d:vclass) : bool =
  begin
    match c, d with
    | Bot, _ -> true
    | _, Top -> true
    | Class c', Class d' ->
       let () = if debug then Format.printf "[subtype] checking '%s' <: '%s'\n" c'.cls.name d'.cls.name in
       let c_inhr = inherits_eq c d in
       (* if c_opt is none, return false *)
       begin
         match c_inhr with
         | None -> false
         | Some Bot -> failwith "[subtype] found malformed class inheriting Bot"
         | Some Top -> failwith "[subtype] should never reach this TOP case"
         | Some (Class vc) ->
            (* Check subtyping on both parameters *)
            (&&)
              (subtype
                 d'.tau_i
                 (subst vc.tau_i c'.tau_o c'.tau_i))
              (subtype
                 (subst vc.tau_o c'.tau_i c'.tau_o)
                 d'.tau_o)
        end
    | _, _ -> false
  end

let subtype_method (ctx:context) (m1:cmethod) (m2:cmethod) : bool =
  let Method (r1, n1, args1) = m1 in
  let Method (r2, n2, args2) = m2 in
  (* m1.returntype <: m2.returntype *)
  let r1_t = vclass_of_ctype r1 ctx in
  let r2_t = vclass_of_ctype r2 ctx in
  let r_ok = subtype r1_t r2_t in
  (* forall args, m2.arg <: m1.arg *)
  let ctx' = context_flip ctx in
  let a_ok =
    ((List.length args1) = (List.length args2))
    && (List.fold_left2 (fun acc (Arg (a1,_)) (Arg (a2,_)) ->
                         acc && subtype (vclass_of_ctype a2 ctx')
                                        (vclass_of_ctype a1 ctx'))
                       true args1 args2)
  in
  and_all [r_ok; a_ok]

(*** well-formedness ***)
(* trivial now, may want to check uniqueness later *)
let param_ok (ctx:context) (c:class_record) : bool =
  true
let rec class_ok_aux (ctx:context) (c:vclass) : bool =
  begin
    match c with
    | Top | Bot -> true
    | Class c' ->
       (* bind param to tau_i tau_o *)
       let ctx' = context_add (c'.cls.param) (CType c'.tau_i) (CType c'.tau_o) ctx in
       let p_ok = param_ok ctx' c' in
       let e_ok = class_ok_aux ctx' (c'.cls.extends) in
       let f_ok = and_all (List.map (field_ok ctx') (c'.cls.fields)) in
       let m_ok = methods_ok ctx' c' in
       and_all [p_ok; e_ok; f_ok; m_ok]
  end
and interface_ok_aux (ctx:context) (i:vinterface) : bool =
  begin
    match i with
    | Interface ir -> failwith "cannot check interface"
  end
and arg_ok (ctx:context) (a:carg) : bool =
  match a with | Arg (t, name) -> type_ok ctx t
and body_ok (ctx:context) : bool =
  true (* TODO want to allow implementations, eventually *)
and field_ok (ctx:context) (f:cfield) : bool =
  match f with | Field (t, name) -> type_ok ctx t
and methods_ok (ctx:context) (c:class_record) : bool =
  (* All methods we possibly overwrite *)
  let extm_set = collect_extends_methods c.cls.extends in
  (* All methods we need to implement. That is, all interface methods minus inherited methods. *)
  let intm_set = MethodSet.diff (collect_implements_methods c.cls.implements) extm_set in
  let clsm_set = MethodSet.of_list c.cls.methods in
  (* CHECK CONDITIONS *)
  (* First, no duplicates *)
  let no_duplicates = (=) (List.length c.cls.methods)
                          (MethodSet.cardinal clsm_set)
  in
  (* Next, all interfaces implemented *)
  let interfaces_implemented = MethodSet.is_empty (MethodSet.diff intm_set clsm_set) in
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
    ) true c.cls.methods
  in
  and_all [no_duplicates; interfaces_implemented; types_ok]
and type_ok (ctx:context) (t:ctype) : bool =
  match t with
  | TVar str -> context_mem str ctx
  | CType c -> class_ok_aux ctx c
  | IType i -> interface_ok_aux ctx i

let class_ok (c:vclass) : bool =
  class_ok_aux empty_context c
let interface_ok (i:vinterface) : bool =
  interface_ok_aux empty_context i
