open Definitions

type variance = Pos | Neg
type t = ClassContext.t
         * ShapeContext.t
         * TypeContext.t
         * variance

(* [init cc sc tc] Initialize a type context from various parts.
   (The parts are all lists for convenience of syntax.) *)
let init (cc:ClassContext.t)
         (sc:ShapeContext.t)
         (tc:TypeContext.t) =
  (cc, sc, tc, Pos)

(* [add_var ctx k v] Add the variable [k] to the current type context,
   replacing old bindings. *)
let add_var (ctx:t) (k:string) (v:type_t * type_t) : t =
  let (cc, sc, tc, vr) = ctx in
  (cc, sc, TypeContext.add_var tc k v, vr)

(* [add_contravariant_var ctx k v] Add the variable [k] to the current
   type context in a contravariant (negative) position. *)
let add_contravariant_var (ctx:t) (k:string) (v:type_t) : t =
  add_var ctx k (v, Top)

(* [add_covariant_var ctx k v] Add the variable [k] to the current
   type context in a covariant (positive) position. *)
let add_covariant_var (ctx:t) (k:string) (v:type_t) : t =
  add_var ctx k (Bot, v)

(* [add_vars ctx vs] Add all bindings in [vs] to the current context,
   replacing existing bindings. *)
let add_vars (ctx:t) (vs:TypeContext.t) : t =
  List.fold_left (fun ctx' (k,v1,v2) -> add_var ctx k (v1, v2)) ctx vs

(* [flip_variance ctx] Reverse the variance of context [ctx]. *)
let flip_variance (ctx:t) : t =
  let (cc, sc, tc, vr) = ctx in
  let vr' = match vnc with | Pos -> Neg | Neg -> Pos in
  (cc, sc, tc, vr')

(* [lookup_tau_i ctx var] Lookup the contravariant type mapped to [var]. *)
let find_tau_i (ctx:t) (var:string) : type_t =
  let (_,_,tc,_) = ctx in
  TypeContext.find_tau_i tc var

(* [lookup_tau_o ctx var] Lookup the COvariant type mapped to [var]. *)
let find_tau_o (ctx:t) (var:string) : type_t =
  let (_,_,tc,_) = ctx in
  TypeContext.find_tau_o tc var

(* [find_class ctx name] Lookup the class with name [name] in context [ctx]. *)
let find_class (ctx:t) (name:string) : sig_t =
  let (cc,_,_,_) = ctx in
  ClassContext.find cc name

let string_of_variance (vr:variance) : string =
  begin match vr with
  | Pos -> "Positive"
  | Neg -> "Negative"
  end

let to_string (ctx:t) : string =
  let (cc, sc, tc, vr) = ctx in
  Format.sprintf "@@@@@@@@@@\nCLASSES = %s\nSHAPES = %s\nVARS = %s\nvariance = %s\n}}@@@@@@@@@@"
                 (ClassContext.to_string cc)
                 (ShapeContext.to_string sc)
                 (TypeContext.to_string  tc)
                 (string_of_variance     vr)
