open Definitions
open Well_formed
open Test_utils

let param = "ITERABLE_PARAM"
let i_iterable =
  Interface ( "Iterable"
            , [param]
            , [] (* extends interfaces *)
            , [] (* satisifies shapes *)
            , [ (NoCond, Method( Instance("Iterator", [])
                               , "getIterator"
                               , [] ))
              ; (NoCond, Method( Instance("Integer", [])
                               , "getLength"
                               , []))
              ; (SuperSat(param, Equatable.s_equatable), Method( Instance("Boolean",[])
                                                               , "contains"
                                                               , [Arg( Super param
                                                                     , "value")]))
              ])

(* reusable tests *)
let test_shape_satisfied (ctx:Context.t) =
  let () = typecheck ctx (I i_iterable) [] in
  let () = check_expr ctx ~expected:(Instance("Iterator", []))
                      ~observed:(Call(("Iterable", []), "getIterator", [])) in
  let () = check_expr ctx ~expected:(Instance("Integer", []))
                      ~observed:(Call(("Iterable", []), "getLength", [])) in
  let () = check_expr ctx ~expected:(Instance("Boolean", []))
                      ~observed:(Call(("Iterable", []), "contains", [("Number", [])])) in
  ()

let test_shape_not_satisfied (ctx:Context.t) =
  let () = typecheck ctx (I i_iterable) [] in
  let () = check_expr ctx ~expected:(Instance("Iterator", []))
                      ~observed:(Call(("Iterable", []), "getIterator", [])) in
  let () = check_expr ctx ~expected:(Instance("Integer", []))
                      ~observed:(Call(("Iterable", []), "getLength", [])) in
  let () = check_method_names ctx ~expected:["getIterator"; "getLength"]
                              ~observed:(Instance("Iterable", [])) in
  ()

(* Experiment with 'super T', be sure the method accepts any supertype of the param *)
let test_super_t (cc:ClassContext.t) =
  let ctx = Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, Instance("Long",[]));
                                                      (param,          Bot, Instance("Long",[]))]) in
  (* A few valid instantiations of 'super T'. These are U ::> Long *)
  let () = check_expr ctx ~expected:(Instance("Boolean", []))
                      ~observed:(Call(("Iterable", []), "contains", [("Long", [])] )) in
  let () = check_expr ctx ~expected:(Instance("Boolean", []))
                      ~observed:(Call(("Iterable", []), "contains", [("Number", [])] )) in
  (* Check invalid instantiations, U :://> Long *)
  let () = check_expr_false ctx ~expected:(Instance("Boolean", []))
                      ~observed:(Call(("Iterable", []), "contains", [("Integer", [])] )) in
  let () = check_expr_false ctx ~expected:(Instance("Boolean", []))
                      ~observed:(Call(("Iterable", []), "contains", [("Iterable", [])] )) in
  let () = check_expr_false ctx ~expected:(Instance("Boolean", []))
                      ~observed:(Call(("Iterable", []), "contains", [("Boolean", [])] )) in
  ()

let () =
  let cc = ClassContext.of_list [I Number.i_integer; I Number.i_number;
                                 I Number.i_long; I Iterator.i_iterator;
                                 I Boolean.i_boolean; I i_iterable] in
  let ctx_sats = [
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, Bot);
                                              (param,          Bot, Bot)])
  ; Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, Instance("Number",[]));
                                              (param,          Bot, Instance("Number", []))])
  ; Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, Instance("Long",[]));
                                              (param,          Bot, Instance("Long", []))])
  ] in
  let ctx_unsats = [
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, Top);
                                              (param,          Bot, Top)])
  ; Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, Instance("Boolean", []));
                                              (param,          Bot, Instance("Boolean", []))])
  ] in
  let () = List.iter test_shape_satisfied ctx_sats in
  let () = List.iter test_shape_not_satisfied ctx_unsats in
  let () = test_super_t cc in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t (List.hd ctx_unsats) (I i_iterable)) in
  ()
