open Definitions

type variance = Pos | Neg
type t = ClassContext.t
         * ShifterContext.t
         * TypeContext.t
         * variance

(* [init cc sc tc] Initialize a type context from various parts.
   (The parts are all lists for convenience of syntax.) *)
let init (cc:ClassContext.t)
         (sc:ShifterContext.t)
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
  let vr' = match vr with | Pos -> Neg | Neg -> Pos in
  (cc, sc, tc, vr')

(* [lookup_tau_i ctx var] Lookup the contravariant type mapped to [var]. *)
let find_tau_i (ctx:t) (var:string) : type_t =
  let (_,_,tc,_) = ctx in
  TypeContext.find_tau_i tc var

(* [lookup_tau_o ctx var] Lookup the COvariant type mapped to [var]. *)
let find_tau_o (ctx:t) (var:string) : type_t =
  let (_,_,tc,_) = ctx in
  TypeContext.find_tau_o tc var

(* [find_sig ctx name] Lookup the class/interface with name [name] in context [ctx]. *)
let find_sig (ctx:t) (name:string) : sig_t =
  let (cc,_,_,_) = ctx in
  ClassContext.find cc name

(* [find_shifted ctx name] Lookup shifters attached to name [name] in context [ctx].

   From these, return the shapes they imply. *)
let find_shifted (ctx:t) (name:string) : shape_t list =
  let (_,sc,_,_) = ctx in
  List.fold_right (fun wr acc -> let Shifter(_,_,shapes) = wr in shapes @ acc)
                  (ShifterContext.find sc name)
                  []
(* [find_shifted_cond ctx nm] Common wrapper -- find the shifted shapes
   attached to [name], wrap these in a dummy condition to keep everything
   consistently guarded. *)
let find_shifted_cond (ctx:t) (name:string) : (cond_t * shape_t) list =
  List.map (fun x -> (NoCond, x)) (find_shifted ctx name)

(* [is_bound ctx name] Check if the variable [name] is bound in the current
   type context. *)
let is_bound (ctx:t) (var:string) : bool =
  let (_,_,tc,_) = ctx in
  begin match TypeContext.find_opt tc var with
  | Some _ -> true
  | None   -> false
  end

(* [set_this_sig_t ctx st] Set the special "THIS" variable to point to the sig [st].  *)
let set_this_sig_t (ctx:t) (st:sig_t) : t =
  let name       = name_of_sig_t st in
  let (_,_,tc,_) = ctx in
  let inst       = Instance(name, tc) in
  add_var ctx "THIS" (inst, inst)

(* [set_this_shape_t ctx st] Set the special "THIS" variable
   to point to the shape [st] (sort of.. it's auto-bound).  *)
let set_this_shape_t (ctx:t) (st:shape_t) : t =
  add_var ctx "THIS" (Bot, Bot)

let string_of_variance (vr:variance) : string =
  begin match vr with
  | Pos -> "Positive"
  | Neg -> "Negative"
  end

let to_string (ctx:t) : string =
  let (cc, sc, tc, vr) = ctx in
  Format.sprintf "@@@@@@@@@@\nCLASSES = %s\nSHAPES = %s\nVARS = %s\nvariance = %s\n}}@@@@@@@@@@"
                 (ClassContext.to_string cc)
                 (ShifterContext.to_string sc)
                 (TypeContext.to_string  tc)
                 (string_of_variance     vr)
