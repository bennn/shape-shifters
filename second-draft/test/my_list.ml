open Definitions
open Well_formed
open Test_utils

let param = "LIST_PARAM"
let i_my_list =
  Interface ( "MyList"
            , [param]
            , [ (NoCond, Indexed.i_indexed)
              ; (Sat(param, Equatable.s_equatable), Container.i_container)]  (* extends interfaces *)
            , [ (Sat(param, Cloneable.s_cloneable), Cloneable.s_cloneable)
              ; (Sat(param, Addable.s_addable)  , Addable.s_addable)] (* satisifies shapes *)
            , [ (NoCond, Method( Bot
                               , "addItem"
                               , [Arg(TVar param, "value")] ))
              ; (SuperSat(param, Cloneable.s_cloneable), Method( Instance("MyList", [(param, Bot, Super param)])
                                                               , "clone"
                                                               , [] ))
              ; (SuperSat(param, Addable.s_addable), Method( Instance("MyList", [(param, Bot, Super param)])
                                                               , "plus"
                                                               , [Arg(Instance("Indexed", [(param, Bot, Super param)]), "that")] ))
              ])

(* TESTS *)
let test_bot cc =
  let bot_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = typecheck bot_ctx (I i_my_list) [] in
  let () = check_method_names bot_ctx ~expected:["addItem"; "clone"; "plus"]
                              ~observed:(Instance("MyList", [])) in
  ()

let test_top cc =
  let top_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (Container.param, TVar param, Bot);
                                              (param,          Bot, Top)]) in
  (* Test methods *)
  let () = typecheck top_ctx (I i_my_list) [] in
  let () = check_method_names top_ctx ~expected:["addItem"]
                              ~observed:(Instance("MyList", [])) in
  ()

let test_long cc =
  let long_ctx =
    Context.init cc []
                 (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                        (Iterable.param, Bot, TVar param);
                                        (Indexed.param, Bot, TVar param);
                                        (Container.param, TVar param, Bot);
                                        (param,          Bot, Instance("Long", []))])
  in
  let () = typecheck long_ctx (I i_my_list) [] in
  let () = check_method_names long_ctx ~expected:["addItem"; "plus"]
                              ~observed:(Instance("MyList", [])) in
  (* Bad calls *)
  let () = check_expr_false long_ctx ~expected:Bot
                            ~observed:(Call(("MyList", []), "clone", [] )) in
  let () = check_expr_false long_ctx ~expected:Bot
                            ~observed:(Call(("MyList", []), "addItem", [("Integer", [])] )) in
  let () = check_expr_false long_ctx ~expected:(Instance("MyList", []))
                            ~observed:(Call(("MyList", []), "plus", [("MyList", [])] )) in
  (* Good calls *)
  let () = check_expr long_ctx ~expected:Bot
                      ~observed:(Call(("MyList", []), "addItem", [("Long", [])] )) in
  let () = check_expr long_ctx ~expected:Bot
                      ~observed:(Call(("MyList", []), "addItem", [("Number", [])] )) in
  let () = check_expr long_ctx ~expected:(Instance("MyList", []))
                      ~observed:(Call(("MyList", []), "plus", [("Indexed", [])] )) in
  ()

let test_boolean cc =
  let boolean_ctx =
    Context.init cc [("Boolean", Dummy_clone.w_dummy_clone)]
                 (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                        (Iterable.param, Bot, TVar param);
                                        (Indexed.param, Bot, TVar param);
                                        (Container.param, TVar param, Bot);
                                        (param,          Bot, Instance("Boolean", []))])
  in
  let () = typecheck boolean_ctx (I i_my_list) [] in
  let () = check_method_names boolean_ctx ~expected:["addItem"; "clone"]
                              ~observed:(Instance("MyList", [])) in
  ()

let () =
  let cc =
    ClassContext.of_list [I Number.i_integer; I Number.i_number;
                          I Number.i_long; I Iterator.i_iterator;
                          I Boolean.i_boolean; I Iterable.i_iterable;
                          I Indexed.i_indexed; I i_my_list] in
  let () = test_bot cc in
  let () = test_top cc in
  let () = test_long cc in
  let () = test_boolean cc in
  (* let () = check_expr *)
  let ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (I i_my_list)) in
  ()
