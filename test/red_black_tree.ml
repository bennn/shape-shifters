open Definitions
open Well_formed
open Test_utils

let param = "RED_BLACK_TREE_PARAM"
let c_red_black_tree =
  Class ( "RedBlackTree"
            , [param]
            , [] (* extends *)
            , [ (NoCond, My_set.i_my_set)] (* inherits *)
            , [ (Sat(param, Cloneable.s_cloneable), Cloneable.s_cloneable);
                (Sat(param, Hashable.s_hashable), Hashable.s_hashable)] (* satisifies shapes *)
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
                , (Method( Instance("Boolean", [])
                         , "equals"
                         , [Arg(Instance("RedBlackTree", [(param, Super param, Super param)]), "that")])
                  , Return Null ))
              ; (SuperSat(param, Cloneable.s_cloneable)
                , (Method( Instance("RedBlackTree", [(param, Bot, Super param)])
                         , "clone"
                         , [])
                  , Return Null ))
              ; (SuperSat(param, Hashable.s_hashable)
                , (Method( Instance("Integer", [])
                         , "hash"
                         , [])
                  , Return Null ))
            ])

let test_bot cc =
  let bot_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (My_set.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = typecheck bot_ctx (C c_red_black_tree) [Sat(param, Comparable.s_comparable)] in
  let () = check_method_names bot_ctx ~expected:["getIterator"; "getLength"; "contains"; "equals"; "clone"; "hash"]
                              ~observed:(Instance("RedBlackTree", [])) in
  ()

let test_top cc =
  let top_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (My_list.param, Bot, TVar param);
                                              (Container.param, TVar param, Bot);
                                              (param,          Bot, Top)]) in
  let () = typecheck_false top_ctx (C c_red_black_tree) [Sat(param, Comparable.s_comparable)] in
  ()

let () =
  let cc =
    ClassContext.of_list [I Number.i_integer; I Number.i_number;
                          I Number.i_long; I Iterator.i_iterator;
                          I Boolean.i_boolean; I Iterable.i_iterable;
                          I My_set.i_my_set;
                          I Container.i_container;
                          C c_red_black_tree] in
  let () = Format.printf "/* [my_red_black_tree] checking RBTree<Bot> has shape methods. */\n" in
  let () = test_bot cc in
  let () = Format.printf "/* [my_red_black_tree] checking RBTree<Top> does not have shape methods. */\n" in
  let () = test_top cc in
  (* test calls *)
  (* Print *)
  let ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (My_set.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = Format.printf "/* [my_red_black_tree] all tests pass!. */\n" in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (C c_red_black_tree)) in
  ()
