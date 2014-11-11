open Definitions
open Well_formed
open Test_utils

let s_equatable =
  Shape ( "Equatable"
        , []
        , [(NoCond, Method( Instance("Boolean", [])
                          , "equals"
                          , [Arg(TVar "THIS", "that")]))])

let () =
  let ctx =
    let cc = ClassContext.of_list [I Boolean.i_boolean] in
    Context.init cc [] []
  in
  let () = assert_true (shape_ok ctx s_equatable) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_shape_t ctx s_equatable) in
  ()
