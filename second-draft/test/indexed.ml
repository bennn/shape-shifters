open Definitions
open Well_formed
open Test_utils

let param = "INDEXED_PARAM"
let i_indexed =
  Interface ( "Indexed"
            , [param]
            , [ (NoCond, Iterable.i_iterable)] (* extends interfaces *)
            , [ (Sat(param, Equatable.s_equatable), Equatable.s_equatable)
              ; (Sat(param, Hashable.s_hashable)  , Hashable.s_hashable)] (* satisifies shapes *)
            , [ (NoCond, Method( TVar param
                               , "get"
                               , [Arg(Instance("Integer", []), "index")] ))
              ; (SuperSat(param, Equatable.s_equatable), Method( Instance("Boolean", [])
                               , "equals"
                               , [Arg (Super param, "value")] ))
              ; (SuperSat(param, Hashable.s_hashable), Method( Instance("Integer",[])
                                                               , "hash"
                                                               , [] ))
              ])

let () =
  let cc = ClassContext.of_list [I Number.i_integer; I Number.i_number;
                                 I Number.i_long; I Iterator.i_iterator;
                                 I Boolean.i_boolean; I Iterable.i_iterable;
                                 I i_indexed] in
  let bot_ctx = Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, Bot);
                                                          (Iterable.param, Bot, Bot);
                                                          (param,          Bot, Bot)]) in
  let top_ctx = Context.init cc [] (TypeContext.of_list  [(Iterator.param, Bot, Top);
                                                          (Iterable.param, Bot, Top);
                                                          (param,          Bot, Top)]) in
  (* Test methods *)
  (* Does not inherit from Iterable, but implementing classes will *)
  let () = check_method_names bot_ctx ~expected:["get"; "equals"; "hash"]
                              ~observed:(Instance("Indexed", [])) in
  let () = check_method_names top_ctx ~expected:["get"]
                              ~observed:(Instance("Indexed", [])) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t bot_ctx (I i_indexed)) in
  ()
