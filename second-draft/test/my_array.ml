open Definitions
open Well_formed
open Test_utils

let param = "ARRAY_PARAM"
let c_my_array =
  Class ( "MyArray"
            , [param]
            , [] (* extends *)
            , [ (NoCond, My_list.i_my_list)] (* inherits *)
            , [ (Sat(param, Cloneable.s_cloneable), Cloneable.s_cloneable)
              ; (Sat(param, Addable.s_addable), Addable.s_addable)] (* satisifies shapes *)
            , [ (NoCond, (Method( Bot
                                , "addItem"
                                , [Arg(TVar param, "value")])
                         , Return Null ))
              ; (NoCond, (Method( TVar param
                                , "get"
                                , [Arg(Instance("Integer", []), "index")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Iterator", [])
                                , "getIterator"
                                , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                                , "getLength"
                                , [])
                         , Return Null ))
              ; (SuperSat(param, Equatable.s_equatable)
                , (Method( Instance("Boolean", [])
                         , "contains"
                         , [Arg(Super param, "value")])
                  , Return Null ))
              ; (SuperSat(param, Hashable.s_hashable)
                , (Method( Instance("Integer", [])
                         , "hash"
                         , [])
                  , Return Null))
              ; (SuperSat(param, Equatable.s_equatable)
                , (Method( Instance("Boolean", [])
                         , "equals"
                         , [Arg(Instance("Indexed", [(param, Super param, Super param)]), "that")])
                  , Return Null ))
              ; (SuperSat(param, Cloneable.s_cloneable)
                , (Method( Instance("MyArray", [(param, Bot, Super param)])
                         , "clone"
                         , [])
                  , Return Null ))
              ; (SuperSat(param, Addable.s_addable)
                , (Method( Instance("MyArray", [(param, Bot, Super param)])
                         , "plus"
                         , [Arg(Instance("Indexed", [(param, Bot, Super param)]), "that")] )
                  , Return Null ))
            ])


let test_bot cc =
  let bot_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (My_list.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = typecheck bot_ctx (C c_my_array) [] in
  let () = check_method_names bot_ctx ~expected:["addItem"; "get"; "getIterator"; "getLength"; "contains"; "hash"; "equals"; "clone"; "plus"]
                              ~observed:(Instance("MyArray", [])) in
  ()

let test_top cc =
  let top_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (My_list.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (Container.param, TVar param, Bot);
                                              (param,          Bot, Top)]) in
  let () = typecheck top_ctx (C c_my_array) [] in
  let () = check_method_names top_ctx ~expected:["addItem"; "get"; "getIterator"; "getLength"]
                              ~observed:(Instance("MyArray", [])) in
  ()

let () =
  let cc =
    ClassContext.of_list [I Number.i_integer; I Number.i_number;
                          I Number.i_long; I Iterator.i_iterator;
                          I Boolean.i_boolean; I Iterable.i_iterable;
                          I Indexed.i_indexed; I My_list.i_my_list;
                          C c_my_array] in
  let () = test_bot cc in
  let () = test_top cc in
  (* TODO test calls *)
  (* Print *)
  let ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (My_list.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (C c_my_array)) in
  ()
