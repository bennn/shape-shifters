(* ARG = type , name *)
type arg_t    = Arg of type_t * string
(* CONDITION = TVAR satisfies SHAPE | super TVAR satisfies SHAPE | NOTHING *)
and  cond_t   = Sat of string * shape_t | SuperSat of string * shape_t | Nothing
(* SHAPE = name , extends (shapes) , methods *)
and  shape_t  = Shape of string * ((cond_t * shape_t) list) * (method_t list)
(* METHOD = condition , return_type , name , args *)
and  method_t = Method of cond_t * type_t * string * (arg_t list)
(* TYPE = variable , instantiation, or the special TOP/BOT *)
and  type_t   = TVar of string
              | Super of string
              | Instance of string * (string -> (type_t * type_t))
              | Top | Bot

(* INTERFACE = name , params, extends, satisfies, methods *)
type sig_t    = Interface of string
                             * (string list)
                             * ((cond_t * sig_t)   list)
                             * ((cond_t * shape_t) list)
                             * (method_t list)
type stmt_t   = Null (* | Return | If | While | ... *)
(* CLASS = name , params, extends , implements , satisfies , methods+bodies *)
type class_t  = Class of string
                         * (string list)
                         * ((cond_t * class_t)  list)
                         * ((cond_t * sig_t)    list)
                         * ((cond_t * shape_t)  list)
                         * ((method_t * stmt_t) list)

(* VALUES = for the 'class table' *)
type value_t = C of class_t | I of sig_t

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
  | Nothing -> "[[nil]]"
  end
let string_of_method_t (md:method_t) : string =
  let Method (_, _, name, _) = md in
  name
let string_of_type_t (tt:type_t) : string =
  begin match tt with
  | TVar v -> Format.sprintf "TVar(%s)" v
  | Super v -> Format.sprintf "Super(%s)" v
  | Instance (name, _) -> name
  | Bot -> "Bottom"
  | Top -> "Top"
  end
let string_of_sig_t (st:sig_t) : string =
  let Interface (name, _, _, _, _) = st in
  name
let string_of_class_t (ct:class_t) : string =
  let Class (name, _, _, _, _, _) = ct in
  name
let string_of_value_t (vt:value_t) : string =
  begin match vt with
  | C c -> string_of_class_t c
  | I i -> string_of_sig_t   i
  end

(* Tables *)
module ValueTable = Set.Make (struct
  type t = value_t
  let compare t1 t2 =
    Pervasives.compare (string_of_value_t t1)
                       (string_of_value_t t2)
end)

module ShapeTable = Set.Make (struct
  type t = shape_t
  let compare s1 s2 =
    Pervasives.compare (string_of_shape_t s1)
                       (string_of_shape_t s2)
end)
