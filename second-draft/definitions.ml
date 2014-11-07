(* ARG = type , name *)
type arg_t    = Arg of type_t * string
(* CONDITION = TVAR satisfies SHAPE | super TVAR satisfies SHAPE | NOTHING *)
and  cond_t   = Sat of string * shape_t | SuperSat of string * shape_t | NoCond
(* SHAPE = name , extends (shapes) , methods *)
and  shape_t  = Shape of string * ((cond_t * shape_t) list) * ((cond_t * method_t) list)
(* METHOD = return_type , name , args *)
and  method_t = Method of type_t * string * (arg_t list)
(* TYPE = variable , instantiation, or the special TOP/BOT *)
and  type_t   = TVar of string
              | Super of string
              | Instance of string * (string -> (type_t * type_t))
              | Top | Bot

(* the inter/class don't need to be recursive, could use strings. I don't think it matters *)
(* INTERFACE = name , params, extends, satisfies, methods *)
type inter_t    = Interface of string
                             * (string list)
                             * ((cond_t * inter_t) list)
                             * ((cond_t * shape_t) list)
                             * ((cond_t * method_t) list)
type stmt_t   = Null (* | Return | If | While | ... *)
(* CLASS = name , params, extends , implements , satisfies , methods+bodies *)
type class_t  = Class of string
                         * (string list)
                         * ((cond_t * class_t)  list)
                         * ((cond_t * inter_t)  list)
                         * ((cond_t * shape_t)  list)
                         * ((cond_t * (method_t * stmt_t)) list)

(* VALUES = for the 'class table' *)
type sig_t = C of class_t | I of inter_t

(* tostring functions *)
let string_of_arg_t (arg:arg_t) : string =
  let Arg (_, name) = arg in
  name
let string_of_shape_t (st:shape_t) : string =
  let Shape (name, _, _) = st in
  name
let string_of_cond_t (ct:cond_t) : string =
  begin match ct with
  | Sat (var, shape) -> Format.sprintf "[[%s SATISFIES %s]]" var (string_of_shape_t shape)
  | SuperSat (var, shape) -> Format.sprintf "[[SUPER %s SATISFIES %s]]" var (string_of_shape_t shape)
  | NoCond -> "[[nil]]"
  end
let string_of_method_t (md:method_t) : string =
  let Method (_, name, _) = md in
  name
let string_of_type_t (tt:type_t) : string =
  begin match tt with
  | TVar v -> Format.sprintf "TVar(%s)" v
  | Super v -> Format.sprintf "Super(%s)" v
  | Instance (name, _) -> name
  | Bot -> "Bottom"
  | Top -> "Top"
  end
let string_of_inter_t (st:inter_t) : string =
  let Interface (name, _, _, _, _) = st in
  name
let string_of_class_t (ct:class_t) : string =
  let Class (name, _, _, _, _, _) = ct in
  name
let string_of_sig_t (vt:sig_t) : string =
  begin match vt with
  | C c -> string_of_class_t c
  | I i -> string_of_inter_t i
  end
let name_of_shape_t (st:shape_t) : string =
  string_of_shape_t st
let name_of_class_t (ct:class_t) : string =
  string_of_class_t ct
let name_of_inter_t (it:inter_t) : string =
  string_of_inter_t it
let name_of_sig_t (vt:sig_t) : string =
  string_of_sig_t vt

(* Tables *)
module StringMap   = Map.Make (String)
type class_context = sig_t StringMap.t
type shape_context = shape_t StringMap.t

(* Context = signatures (types), shapes, and type vars *)
type variance = Pos | Neg
type context  = class_context
               * shape_context
               * (variance * (string -> (type_t * type_t)))
let context_init (cc:class_context) (sc:shape_context) (varmap:(string -> (type_t * type_t))) =
  (cc, sc, (Pos, varmap))
let flip_variance (ctx:context) : context =
  let (cc, sc, (vnc, varmap)) = ctx in
  let vnc' = match vnc with | Pos -> Neg | Neg -> Pos in
  (cc, sc, (vnc', varmap))
let lookup_tau_i (ctx:context) (var:string) : type_t =
  let (_,_,(vnc, vm)) = ctx in
  begin match vnc with
  | Pos -> fst (vm var)
  | Neg -> snd (vm var)
  end
let lookup_tau_o (ctx:context) (var:string) : type_t =
  let (_,_,(vnc, vm)) = ctx in
  begin match vnc with
  | Neg -> fst (vm var)
  | Pos -> snd (vm var)
  end
