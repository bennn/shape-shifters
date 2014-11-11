(* TYPE = variable , instantiation, or the special TOP/BOT *)
type type_t   = TVar of string
              | Super of string
              | Instance of val_t
              | Top | Bot
(* VALUE = class/interface name + typevars *)
and  val_t        = string * type_context
and  type_context = (string * type_t * type_t) list
(** Rep. Invariant: list length <= number of instance parameters *)

(* ARG = type , name *)
type arg_t    = Arg of type_t * string
(* METHOD = return_type , name , args *)
type method_t = Method of type_t * string * (arg_t list)

(* CONDITION = TVAR satisfies SHAPE | super TVAR satisfies SHAPE | NOTHING *)
type cond_t   = Sat of string * shape_t
              | SuperSat of string * shape_t (* TODO idk... *)
              | NoCond
(* SHAPE = name , extends (shapes) , methods *)
and  shape_t  = Shape of string
                         * ((cond_t * shape_t) list)
                         * ((cond_t * method_t) list)

(* the inter/class don't need to be recursive, could use strings. I don't think it matters *)
(* INTERFACE = name , params, extends, satisfies, methods *)
type inter_t    = Interface of string
                             * (string list)
                             * ((cond_t * inter_t) list)
                             * ((cond_t * shape_t) list)
                             * ((cond_t * method_t) list)
type expr_t   = Null
              | New  of val_t
              | Call of val_t  * string * (val_t list) (* object, method name, args *)
              | ExtM of string * string * (val_t list) (* class name, method name, args *)
type stmt_t   = Return of expr_t (* | If | While | ... *)

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
  | NoCond -> "[[true]]"
  end
let string_of_type_t (tt:type_t) : string =
  begin match tt with
  | TVar v -> Format.sprintf "TVar(%s)" v
  | Super v -> Format.sprintf "Super(%s)" v
  | Instance (name, _) -> name
  | Bot -> "Bottom"
  | Top -> "Top"
  end
let string_of_method_t (md:method_t) : string =
  let Method (rtype, name, _) = md in
  Format.sprintf "%s %s(...)" (string_of_type_t rtype) name
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
  let Shape (name, _, _) = st in
  name
let name_of_inter_t (it:inter_t) : string =
  let Interface (name, _, _, _, _) = it in
  name
let name_of_class_t (ct:class_t) : string =
  let Class (name, _, _, _, _, _) = ct in
  name
let name_of_sig_t (vt:sig_t) : string =
  begin match vt with
  | C c -> string_of_class_t c
  | I i -> string_of_inter_t i
  end

let string_of_list (f:'a -> string) (xs:'a list) : string =
  Format.sprintf "[%s]" (String.concat "; " (List.map f xs))

(* little helpers *)

(* [params_of_sig_t st] Return the type parameter list attached
   to class/interface [st]. *)
let params_of_sig_t (st:sig_t) : string list =
  begin match st with
  | C (Class (_,tvs,_,_,_,_)) -> tvs
  | I (Interface (_,tvs,_,_,_)) -> tvs
  end

(* [method_sigs_of_sig_t st] Return the method signatures (and conditions) attached
   to class/interface [st]. *)
let method_sigs_of_sig_t (st:sig_t) : (cond_t * method_t) list =
  begin match st with
  | C (Class(_,_,_,_,_,ms))   -> List.map (fun (cond,(m_sig, _)) -> (cond, m_sig)) ms
  | I (Interface(_,_,_,_,ms)) -> ms
  end

(* [for_all2 f xs ys] Assert that the lists [xs] and [ys] have the same length,
   and also that [f x y] holds for each pair [x,y] in [zip xs ys].
   This function is declared HERE because I don't use it elsewhere. *)
let rec for_all2 (f:'a -> 'b -> bool) (xs:'a list) (ys:'b list) : bool =
  begin match xs , ys with
  | [] , []         -> true
  | h1::t1 , h2::t2 -> f h1 h2 && for_all2 f t1 t2
  | _ , _           -> false
  end
