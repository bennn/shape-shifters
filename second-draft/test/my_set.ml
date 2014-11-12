open Definitions
open Well_formed
open Test_utils

let param = "SET_PARAM"
let i_my_set =
  Interface ( "MySet"
            , [param]
            , [ (NoCond, Iterable.i_iterable)
              ; (NoCond, Container.i_container)]
            , [ (NoCond, Equatable.s_equatable)] (* satisifies shapes *)
            , [])

let () =
  let cc =
    ClassContext.of_list [I Number.i_integer; I Number.i_number;
                         I Number.i_long; I Iterator.i_iterator;
                         I Boolean.i_boolean; I Iterable.i_iterable;
                         I Indexed.i_indexed; I i_my_set] in
  let bot_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (Container.param, TVar param, Top);
                                              (param,          Bot, Bot)]) in
  let () = typecheck bot_ctx (I i_my_set) [] in
  let () = check_method_names bot_ctx ~expected:[]
                              ~observed:(Instance("MySet", [])) in
  let top_ctx =
    Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, TVar param);
                                              (Iterable.param, Bot, TVar param);
                                              (Indexed.param, Bot, TVar param);
                                              (Container.param, TVar param, Bot);
                                              (param,          Bot, Top)]) in
  (* Test methods *)
  let () = typecheck top_ctx (I i_my_set) [] in
  let () = check_method_names top_ctx ~expected:[]
                              ~observed:(Instance("MySet", [])) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t bot_ctx (I i_my_set)) in
  ()
