open Definitions
open Well_formed
open Test_utils

let s_comparable =
  Shape ( "Comparable"
        , [(NoCond, Equatable.s_equatable)]
        , [(NoCond, Method( Instance("Boolean", [])
                          , "leq"
                          , [Arg(TVar "THIS", "that")]))])

let () =
  let ctx =
    let cc = ClassContext.of_list [I Boolean.i_boolean] in
    Context.init cc [] []
  in
  let () = assert_true (shape_ok ctx s_comparable) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_shape_t ctx s_comparable) in
  ()
