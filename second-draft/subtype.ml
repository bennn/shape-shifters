open Definitions

(* Context = signatures (types), shapes, and type vars *)
type context = class_context
               * shape_context
               * (string -> (type_t * type_t))

let cDEBUG = false

(* [update_vars vm ctx] update the var map in the context [ctx] to first look in [vm] *)
let update_vars (vm:string -> (type_t * type_t)) (ctx:context) : context =
  let class_c, shape_c, var_c = ctx in
  let var_c' = fun v -> (try vm v with _ -> var_c v) in
  (class_c, shape_c, var_c')

(* [inherits ctx t1 t2] Search valid parents of [t1] for the name of [t2].
   Validity determined by the context [t1]. *)
let rec inherits (ctx:context) (t1:type_t) (t2:type_t) : sig_t option =
  let () = if cDEBUG then Format.printf "[inherits] '%s' <:: '%s'\n" (string_of_type_t t1) (string_of_type_t t2) in
  let class_c, _, _ = ctx in
  begin match t1 , t2 with
  | Instance (name1, varmap1) , Instance (name2, varmap2) ->
     if name1 = name2 then None
     else (* Search all valid parents. *)
       let parent_sigs =
         begin match StringMap.find name1 class_c with
         | C (Class (_, _, exts, _, _, _))  ->
            List.map (fun (p,c) -> (p, C c)) exts
         | I (Interface (_, _, iexts, _, _)) ->
            List.map (fun (p,i) -> (p, I i)) iexts
         end
       in
       (* Helper function for the fold *)
       let search_parents acc (cond, parent) =
         begin match acc with
         | Some _ -> acc
         | None   ->
            if not (condition_ok cond (update_vars varmap1 ctx)) then None
            else let n' = name_of_sig_t parent in
                 if n' = name2
                 then Some parent
                 else inherits ctx (Instance(n', varmap1)) t2
         end
       in
       List.fold_left search_parents None parent_sigs
  | _ -> None
  end
(* [condition_ok cond ctx] Check if the 'satisfies' condition [cond] holds
   in context [ctx]. *)
and condition_ok (cond:cond_t) (ctx:context) : bool =
  let () = if cDEBUG then Format.printf "[condition_ok] \n" in
  let _, _, var_c = ctx in
  begin match cond with
  | Sat (v, shape)
  | SuperSat (v, shape) -> satisfies (snd (var_c v)) shape ctx
  | NoCond -> true
  end
(* [satisfies tt shp ctx] Check if type [tt] is declared to satisfy shape [shp]
   in the current context [ctx]. *)
and satisfies (tt:type_t) (shp:shape_t) (ctx:context) : bool =
  let () = if cDEBUG then Format.printf "[satisfies] \n" in
  let class_c, _, var_c = ctx in
  begin match tt with
  | Bot | Top -> false
  | TVar v
  | Super v -> satisfies (snd (var_c v)) shp ctx
  | Instance (name, vm') ->
     let shapes =
       begin match StringMap.find name class_c with
       | C (Class (_,_,_,_,shps,_)) -> shps
       | I (Interface (_,_,_,shps,_)) -> shps
       end
     in
     shape_satisfies shapes shp ctx
  end
(* [shape_satisfies shapes shp ctx] Depth-first search the inheritance hierarchy of
   the list of shapes [shapes] to find any one that satisfies [shp] in context [ctx]. *)
and shape_satisfies (shapes:(cond_t * shape_t) list) (shp:shape_t) (ctx:context) : bool =
  let Shape(shp_name,_,_) = shp in
  begin match shapes with
  | [] -> false
  | (cond, shp')::tl ->
     let Shape (shp_name', shapes',_) = shp' in
     (* If condition fails, recurse on tail *)
     (* Else check for a match. Failing that search parents. Failing both, recurse. *)
     if not (condition_ok cond ctx)
     then shape_satisfies tl shp ctx
     else shp_name = shp_name' ||
            (shape_satisfies shapes' shp ctx || shape_satisfies tl shp ctx)
  end

(* [inherits_eq ctx t1 t2] Follow the inheritance hierarchy, see if
   any (valid in [ctx]) parent extends the type [t2]. Return the [sig_t] if so,
   else return [None].
   The function [subtype] depends on this: need the name and param name
   of the parent. *)
let inherits_eq (ctx:context) (t1:type_t) (t2:type_t) : sig_t option =
  let () = if cDEBUG then Format.printf "[inherits_eq] '%s' <::= '%s'\n" (string_of_type_t t1) (string_of_type_t t2) in
  let class_c, _, _ = ctx in
  begin match t1, t2 with
  | Instance (name1, varmap1), Instance (name2, varmap2) ->
     if name1 = name2
     then Some (StringMap.find name1 class_c)
     else inherits ctx t1 t2
  | _ -> None (* because this function is guarded by subtype *)
  end

let rec subtype (ctx:context) (t1:type_t) (t2:type_t) : bool =
  let () = if cDEBUG then Format.printf "[subtype] '%s' <: '%s'\n" (string_of_type_t t1) (string_of_type_t t2) in
  let _, _, var_c = ctx in
  begin match t1, t2 with
  (* Easy cases: bot/top *)
  | Bot, _ -> true
  | _, Top -> true
  | Top, _ -> false
  | _, Bot -> false
  (* Resolve type variables *)
  | TVar v , _ ->
     subtype ctx (snd (var_c v)) t2
  | _, TVar v ->
     subtype ctx t1 (snd (var_c v))
  (* supers are a little scary, take the lower bound. FOR NOW. *)
  | Super v , _ ->
     subtype ctx (snd (var_c v)) t2
  | _ , Super v ->
     subtype ctx t1 (snd (var_c v))
  (* the real deal *)
  | Instance (name1, varmap1), Instance (name2, varmap2) ->
     begin match inherits_eq ctx t1 t2 with
     | None -> false
     | Some r ->
        let tvars =
          begin match r with
          | C (Class (_,tvs,_,_,_,_)) -> tvs
          | I (Interface (_,tvs,_,_,_)) -> tvs
          end
        in
        List.fold_left
          (fun all_pass var -> all_pass && ( (* TODO scared about t_i t_o *)
                                 let t_i  = fst (varmap2 var) in
                                 let t_o  = snd (varmap2 var) in
                                 let t_i' = fst (varmap2 var) in
                                 let t_o' = snd (varmap2 var) in
                                 subtype ctx t_i' t_i && subtype ctx t_o t_o'))
          true tvars
     end
  end

let rec subtype_method (ctx:context) (m1:method_t) (m2:method_t) : bool =
  failwith "nope"
