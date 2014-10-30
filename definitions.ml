let debug = false

type  cname    = string
and  cfield   = Field of ctype * cname
and  carg     = Arg   of ctype * cname
and  cmethod  = Method of ctype * cname * carg list * cstmt * ctype
and  cstmt    = Return of cexpr (* Seq of stmt list | Assign of  *)
and  cexpr    = Var of cname | New of class_record
and  ctype    = TVar of string | TClass of class_sig
and  class_sig = { name : cname
                ; param : ctype
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

let class_ok (c:vclass) : bool =
  (* needs context? *)
  failwith ""
