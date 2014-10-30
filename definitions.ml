let debug = false

(*** typedefs ***)
type  cname    = string
and  cfield   = Field of ctype * cname
and  carg     = Arg   of ctype * cname
and  cmethod  = Method of ctype * cname * carg list * cstmt
and  cstmt    = Return of cexpr (* Seq of stmt list | Assign of  *)
and  cexpr    = Var of cname | New of class_record
and  ctype    = TVar of string | CType of vclass
and  class_sig = { name : cname
                ; param : cname
                ; extends : vclass
                (* ; implements : cinterface *)
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

(*** context ***)
(* Kind Context maps type variables *)
type kind_context = (string * ctype * ctype) list
(* 2014-10-29 the whole context might have more parts, later *)
type context = kind_context
let empty_context = []
let rec context_lookup k ctx =
  match ctx with
  | [] -> failwith (Format.sprintf "unbound variable '%s'" k)
  | (k',_,tau_o)::tl -> if k = k' then tau_o else context_lookup k tl
let rec context_add k tau_i tau_o ctx =
  match ctx with
  | [] -> (k, tau_i, tau_o) :: ctx
  | (k',t',t'') :: tl ->
     let rst = context_add k tau_i tau_o tl in
     if k = k'
     then failwith "[context_add] DUPLICATE KEY"
     else (k',t',t'') :: rst
let rec context_mem k ctx =
  match ctx with
  | [] -> false
  | (k',_,_)::tl -> k = k' || context_mem k tl
let rec context_flip ctx =
  match ctx with
  | [] -> []
  | (k, tau_i, tau_o) :: tl -> (k, tau_o, tau_i) :: context_flip tl

let and_all f xs = List.fold_left (fun acc x -> acc && f x) true  xs
let or_all  f xs = List.fold_left (fun acc x -> acc || f x) false xs

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
       let f_ok = and_all (field_ok ctx') (c'.cls.fields) in
       let m_ok = and_all (method_ok ctx') (c'.cls.methods) in
       and_all (fun x -> x) [p_ok; e_ok; f_ok; m_ok]
  end
and arg_ok (ctx:context) (a:carg) : bool =
  match a with | Arg (t, name) -> type_ok ctx t
and body_ok (ctx:context) (b:cstmt) : bool =
  true (* TODO want to allow implementations, eventually *)
and field_ok (ctx:context) (f:cfield) : bool =
  match f with | Field (t, name) -> type_ok ctx t
and method_ok (ctx:context) (m:cmethod) : bool =
  match m with
  | Method (ret_type, name, args, body) ->
     let r_ok   = type_ok ctx ret_type in
     let rgs_ok = and_all (arg_ok (context_flip ctx)) args in
     let b_ok   = body_ok ctx body in
     and_all (fun x -> x) [r_ok; rgs_ok; b_ok]
and type_ok (ctx:context) (t:ctype) : bool =
  match t with
  | TVar str -> context_mem str ctx
  | CType c -> class_ok_aux ctx c

let class_ok (c:vclass) : bool =
  class_ok_aux empty_context c

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
