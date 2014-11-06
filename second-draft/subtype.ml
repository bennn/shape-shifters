open Definitions

(* Context = signatures (types), shapes, and type vars *)
type context = sig_t SigTable.t
               * shape_t ShapeTable.t
               * (string -> (type_t * type_t))

let cDEBUG = false

let update_vars (vm:string -> (type_t * type_t)) (ctx:context) : context =
  let class_c, shape_c, var_c = ctx in
  let var_c' = fun v -> (try vm v with _ -> var_c v) in
  (class_c, shape_c, var_c')

let rec inherits (ctx:context) (t1:type_t) (t2:type_t) : sig_t option =
  let () = if cDEBUG then Format.printf "[inherits] '%s' <:: '%s'\n" (string_of_type_t t1) (string_of_type_t t2) in
  let class_c, _, _ = ctx in
  begin match t1 , t2 with
  | Instance (name1, varmap1) , Instance (name2, varmap2) ->
     if name1 = name2 then None
     else (* Search all valid parents. Sorry about the duplicate code here. *)
       begin match SigTable.find name1 class_c with
       | C (Class (_, _, exts, _, _, _))  ->
          List.fold_left (fun acc (cond, cls) ->
                          begin match acc with
                          | Some _ -> acc
                          | None   ->
                             if not (condition_ok cond (update_vars varmap1 ctx)) then None
                             else (
                               let Class(n',_,_,_,_,_) = cls in
                               if n' = name2
                               then Some (C cls)
                               else inherits ctx (Instance(n', varmap1)) t2
                             )
                          end
                         ) None exts
       | I (Interface (_, _, iexts, _, _)) ->
          List.fold_left (fun acc (cond, inf) ->
                          begin match acc with
                          | Some _ -> acc
                          | None   ->
                             if not (condition_ok cond (update_vars varmap1 ctx)) then None
                             else (
                               let Interface(n',_,_,_,_) = inf in
                               if n' = name2
                               then Some (I inf)
                               else inherits ctx (Instance(n', varmap1)) t2
                             )
                          end
                         ) None iexts
       end
  | _ -> None
  end
and condition_ok (cond:cond_t) (ctx:context) : bool =
  let () = if cDEBUG then Format.printf "[condition_ok] \n" in
  let _, _, var_c = ctx in
  begin match cond with
  | Sat (v, shape)
  | SuperSat (v, shape) -> satisfies (snd (var_c v)) shape ctx
  | Nothing -> true
  end
and satisfies (tt:type_t) (shp:shape_t) (ctx:context) : bool =
  let () = if cDEBUG then Format.printf "[satisfies] \n" in
  let class_c, _, var_c = ctx in
  begin match tt with
  | Bot | Top -> false
  | TVar v
  | Super v -> satisfies (snd (var_c v)) shp ctx
  | Instance (name, vm') ->
     let shapes =
       begin match SigTable.find name class_c with
       | C (Class (_,_,_,_,shps,_)) -> shps
       | I (Interface (_,_,_,shps,_)) -> shps
       end
     in
     shape_satisfies shapes shp ctx
  end
and shape_satisfies (shapes:(cond_t * shape_t) list) (shp:shape_t) (ctx:context) : bool =
  (* TODO tame this helper function *)
  let Shape(shp_name,_,_) = shp in
  List.fold_left (fun acc (cond, shp') -> acc ||
                  begin
                    let Shape (shp_name',shapes',_) = shp' in
                    if not (condition_ok cond ctx) then false
                    else if shp_name = shp_name'   then true
                    else (* check supers *) shape_satisfies shapes' shp ctx
                  end) false shapes

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
     then Some (SigTable.find name1 class_c)
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
  (* supers are a little scary *)
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
