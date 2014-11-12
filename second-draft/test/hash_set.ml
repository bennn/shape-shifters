open Definitions
open Well_formed
open Test_utils

let param = "HASH_SET_PARAM"
let c_hash_set =
  Class ( "HashSet"
            , [param]
            , [] (* extends *)
            , [ (NoCond, My_set.i_my_set)] (* inherits *)
            , [ (Sat(param, Cloneable.s_cloneable), Cloneable.s_cloneable)] (* satisifies shapes *)
            , [
               (NoCond, (Method( Instance("Iterator", [])
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
              ; (SuperSat(param, Equatable.s_equatable)
                , (Method( Instance("Boolean", []) (* TODO set should have a more specific equals *)
                         , "equals"
                         , [Arg(Instance("Container", [(param, Super param, Super param)]), "that")])
                  , Return Null ))
              ; (SuperSat(param, Cloneable.s_cloneable)
                , (Method( Instance("HashSet", [(param, Bot, Super param)])
                         , "clone"
                         , [])
                  , Return Null ))
            ])

let test_bot cc =
  let bot_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (My_set.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = typecheck bot_ctx (C c_hash_set) [Sat(param, Equatable.s_equatable)] in
  let () = check_method_names bot_ctx ~expected:["getIterator"; "getLength"; "contains"; "equals"; "clone"]
                              ~observed:(Instance("HashSet", [])) in
  ()

let test_top cc =
  let top_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (My_list.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (Container.param, TVar param, Bot);
                                              (param,          Bot, Top)]) in
  let () = typecheck_false top_ctx (C c_hash_set) [Sat(param, Equatable.s_equatable)] in
  ()

let () =
  let cc =
    ClassContext.of_list [I Number.i_integer; I Number.i_number;
                          I Number.i_long; I Iterator.i_iterator;
                          I Boolean.i_boolean; I Iterable.i_iterable;
                          I Indexed.i_indexed; I My_set.i_my_set;
                          I Container.i_container;
                          C c_hash_set] in
  let () = test_bot cc in
  let () = test_top cc in
  (* TODO test calls *)
  (* Print *)
  let ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (My_set.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (C c_hash_set)) in
  ()
