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


let () =
  let ctx =
    let cc = ClassContext.of_list [I Number.i_integer; I Iterator.i_iterator;
                                   I Boolean.i_boolean; I i_iterable] in
    let tc = TypeContext.of_list  [(Iterator.param, Bot, Bot);
                                   (param, Bot, Bot)] in
    Context.init cc [] tc
  in
  let () = typecheck ctx (I i_iterable) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (I i_iterable)) in
  (* TODO test when shape is/isn't satisfied, does contains exist *)
  (* TODO test super T on contains *)
  (* TODO test calls to contains *)
  ()
